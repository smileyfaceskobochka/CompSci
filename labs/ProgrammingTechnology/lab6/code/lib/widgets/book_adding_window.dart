import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../database.dart'; // Обновленный путь
import '../providers.dart'; // Обновленный путь
import '../validators/book_validator.dart'; // Обновленный путь

class BookAddingWindow extends ConsumerStatefulWidget {
  const BookAddingWindow({super.key});

  @override
  ConsumerState<BookAddingWindow> createState() => _BookAddingWindowState();
}

class _BookAddingWindowState extends ConsumerState<BookAddingWindow> {
  final _formKey = GlobalKey<FormState>();

  final TextEditingController _titleController = TextEditingController();
  final TextEditingController _genreController = TextEditingController();
  final TextEditingController _publicationYearController = TextEditingController();
  final TextEditingController _isbnController = TextEditingController();

  final BookValidator _validator = BookValidator();

  @override
  void dispose() {
    _titleController.dispose();
    _genreController.dispose();
    _publicationYearController.dispose();
    _isbnController.dispose();
    super.dispose();
  }

  void _submit() async {
    if (_formKey.currentState!.validate()) {
      try {
        final book = Book(
          title: _titleController.text,
          publicationYear: int.parse(_publicationYearController.text),
          genre: _genreController.text,
          isbn: _isbnController.text,
        );

        await ref.read(booksProvider.notifier).addBook(book); // Изменено имя провайдера

        _formKey.currentState!.reset();
        _titleController.clear();
        _genreController.clear();
        _publicationYearController.clear();
        _isbnController.clear();

        if (mounted) {
          ScaffoldMessenger.of(
            context,
          ).showSnackBar(const SnackBar(content: Text('Книга добавлена!'))); // Изменен текст
        }
      } catch (e) {
        if (mounted) {
          ScaffoldMessenger.of(
            context,
          ).showSnackBar(SnackBar(content: Text('Ошибка: $e')));
        }
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      padding: const EdgeInsets.all(20.0),
      child: Form(
        key: _formKey,
        child: Column(
          children: [
            TextFormField(
              controller: _titleController,
              decoration: const InputDecoration(
                labelText: 'Название',
                hintText: 'Введите название книги',
              ),
              validator: _validator.validateTitle,
            ),
            const SizedBox(height: 16), // Добавлен SizedBox для отступа
            TextFormField(
              controller: _genreController,
              decoration: const InputDecoration(
                labelText: 'Жанр',
                hintText: 'Введите жанр книги',
              ),
              validator: _validator.validateGenre,
            ),
            const SizedBox(height: 16),
            TextFormField(
              controller: _publicationYearController,
              keyboardType: TextInputType.number,
              decoration: const InputDecoration(
                labelText: 'Год публикации',
                hintText: 'Введите год публикации',
              ),
              validator: _validator.validatePublicationYear,
            ),
            const SizedBox(height: 16),
            TextFormField(
              controller: _isbnController,
              decoration: const InputDecoration(
                labelText: 'ISBN',
                hintText: 'Введите ISBN',
              ),
              validator: _validator.validateISBN,
            ),
            const SizedBox(height: 24), // Увеличен отступ
            ElevatedButton(
              onPressed: _submit,
              style: ElevatedButton.styleFrom(
                minimumSize: const Size.fromHeight(50), // Увеличен размер кнопки
              ),
              child: const Text('Добавить книгу'), // Изменен текст
            ),
            const SizedBox(height: 10),
            TextButton( // Изменено на TextButton для тестовой кнопки
              onPressed: () {
                _isbnController.text = '978-3-16-148410-0';
                _genreController.text = 'Фантастика';
                _publicationYearController.text = '1999';
                _titleController.text = 'Путешествие на Луну';
              },
              child: const Text('Заполнить тестовой книгой'), // Изменен текст
            ),
          ],
        ),
      ),
    );
  }
}