import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../database.dart'; // Обновленный путь
import '../providers.dart'; // Обновленный путь
import '../validators/author_validator.dart'; // Обновленный путь

class AuthorAddingWindow extends ConsumerStatefulWidget {
  const AuthorAddingWindow({super.key});

  @override
  ConsumerState<AuthorAddingWindow> createState() => _AuthorAddingWindowState();
}

class _AuthorAddingWindowState extends ConsumerState<AuthorAddingWindow> {
  final _formKey = GlobalKey<FormState>();

  final _firstNameController = TextEditingController();
  final _lastNameController = TextEditingController();
  final _nationalityController = TextEditingController();
  final _birthYearController = TextEditingController();

  Book? selectedBook; // Изменено с Dog на Book
  final AuthorValidator _validator =
      AuthorValidator(); // Изменено имя валидатора

  @override
  void dispose() {
    _firstNameController.dispose();
    _lastNameController.dispose();
    _nationalityController.dispose();
    _birthYearController.dispose();
    super.dispose();
  }

  void _submitForm() async {
    if (_formKey.currentState!.validate()) {
      try {
        final author = Author(
          bookId: selectedBook?.id, // Изменено с dogId на bookId
          firstName: _firstNameController.text,
          lastName: _lastNameController.text,
          nationality: _nationalityController.text,
          birthYear: int.parse(_birthYearController.text),
        );

        await ref
            .read(authorsProvider.notifier)
            .addAuthor(author); // Изменено имя провайдера

        _formKey.currentState!.reset();
        _firstNameController.clear();
        _lastNameController.clear();
        _nationalityController.clear();
        _birthYearController.clear();
        setState(() {
          selectedBook = null; // Изменено с selectedDog на selectedBook
        });

        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(content: Text("Автор добавлен!")),
          ); // Изменен текст
        }
      } catch (e) {
        if (mounted) {
          ScaffoldMessenger.of(
            context,
          ).showSnackBar(SnackBar(content: Text("Ошибка: $e")));
        }
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    final booksList = ref.watch(
      booksProvider,
    ); // Изменено с dogsProvider на booksProvider

    return SingleChildScrollView(
      // Обертка в SingleChildScrollView
      padding: const EdgeInsets.fromLTRB(20, 20, 20, 10),
      child: Form(
        key: _formKey,
        child: Column(
          children: [
            TextFormField(
              controller: _firstNameController,
              decoration: const InputDecoration(labelText: 'Имя'),
              validator: _validator.validateFirstName,
            ),
            const SizedBox(height: 16),
            TextFormField(
              controller: _lastNameController,
              decoration: const InputDecoration(labelText: 'Фамилия'),
              validator: _validator.validateLastName,
            ),
            const SizedBox(height: 16),
            TextFormField(
              controller: _nationalityController,
              decoration: const InputDecoration(labelText: 'Национальность'),
              validator: _validator.validateNationality,
            ),
            const SizedBox(height: 16),

            booksList.when(
              loading: () => const CircularProgressIndicator(),
              error: (err, st) =>
                  Text('Ошибка загрузки книг: $err'), // Обновлен текст ошибки
              data: (books) {
                final List<DropdownMenuItem<Book?>> menuItems = [
                  const DropdownMenuItem<Book?>(
                    value: null,
                    child: Text('Нет (без книги)'), // Изменен текст
                  ),
                ];
                menuItems.addAll(
                  books.map((book) {
                    // Изменено с dog на book
                    return DropdownMenuItem<Book?>(
                      // Изменено с Dog на Book
                      value: book,
                      child: Text(
                        '${book.title} (${book.genre})',
                      ), // Обновлен вывод информации о книге
                    );
                  }),
                );
                return DropdownButtonFormField<Book?>(
                  // Изменено с Dog на Book
                  decoration: const InputDecoration(
                    labelText: 'Книга автора', // Изменен текст
                  ),
                  initialValue: selectedBook, // Изменено на initialValue
                  items: menuItems,
                  onChanged: (value) => setState(
                    () => selectedBook = value,
                  ), // Изменено с selectedDog на selectedBook
                );
              },
            ),
            const SizedBox(height: 16),
            TextFormField(
              controller: _birthYearController,
              keyboardType: TextInputType.number,
              decoration: const InputDecoration(labelText: 'Год рождения'),
              validator: _validator.validateBirthYear,
            ),
            const SizedBox(height: 24),
            ElevatedButton(
              onPressed: _submitForm,
              style: ElevatedButton.styleFrom(
                minimumSize: const Size.fromHeight(50),
              ),
              child: const Text("Добавить автора"), // Изменен текст
            ),
            const SizedBox(height: 10),
            TextButton(
              // Изменено на TextButton
              onPressed: () {
                _birthYearController.text = '1828';
                _nationalityController.text = 'Русский';
                _lastNameController.text = 'Толстой';
                _firstNameController.text = 'Лев';
              },
              child: const Text('Заполнить тестовым автором'), // Изменен текст
            ),
          ],
        ),
      ),
    );
  }
}
