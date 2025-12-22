import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../database.dart'; // Обновленный путь
import '../providers.dart'; // Обновленный путь
import '../validators/book_validator.dart'; // Обновленный путь
import 'dart:async';

void showBookEditingDialog(BuildContext context, Book book, WidgetRef ref) {
  // Изменено название функции и тип данных
  final titleCtrl = TextEditingController(
    text: book.title,
  ); // Изменено с breed на title
  final genreCtrl = TextEditingController(
    text: book.genre,
  ); // Изменено с age на genre
  final publicationYearCtrl = TextEditingController(
    text: '${book.publicationYear}',
  ); // Изменено с name на publicationYear
  final isbnCtrl = TextEditingController(
    text: book.isbn,
  ); // Изменено с collar на isbn

  final focusNode = FocusNode();
  String? titleError; // Изменено
  String? genreError; // Изменено
  String? publicationYearError; // Изменено
  String? isbnError; // Добавлено

  final BookValidator validator = BookValidator(); // Изменено имя валидатора

  void closeDialog() {
    if (context.mounted) {
      Navigator.pop(context);
    }
  }

  showDialog(
    context: context,
    builder: (context) => StatefulBuilder(
      builder: (context, setState) => Dialog(
        shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(16)),
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              Text(
                'Редактирование книги', // Изменен заголовок
                style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              ),
              const SizedBox(height: 16),

              // Поле "Название"
              TextField(
                controller: titleCtrl,
                onChanged: (value) {
                  setState(() {
                    titleError = validator.validateTitle(value);
                  });
                },
                focusNode: focusNode,
                decoration: InputDecoration(
                  errorText: titleError,
                  labelText: 'Название',
                  hintText: 'Введите название книги',
                  border: OutlineInputBorder(),
                ),
              ),

              const SizedBox(height: 12),

              // Поле "Жанр"
              TextField(
                controller: genreCtrl,
                onChanged: (value) {
                  setState(() {
                    genreError = validator.validateGenre(value);
                  });
                },
                decoration: InputDecoration(
                  errorText: genreError,
                  labelText: 'Жанр',
                  hintText: 'Введите жанр',
                  border: OutlineInputBorder(),
                ),
              ),

              const SizedBox(height: 12),

              // Поле "Год публикации"
              TextField(
                controller: publicationYearCtrl,
                keyboardType: TextInputType.number,
                onChanged: (value) {
                  setState(() {
                    publicationYearError = validator.validatePublicationYear(
                      value,
                    );
                  });
                },
                decoration: InputDecoration(
                  errorText: publicationYearError,
                  labelText: 'Год публикации',
                  hintText: 'Введите год публикации',
                  border: OutlineInputBorder(),
                ),
              ),

              const SizedBox(height: 12),

              // Поле "ISBN"
              TextField(
                controller: isbnCtrl,
                onChanged: (value) {
                  setState(() {
                    isbnError = validator.validateISBN(value);
                  });
                },
                decoration: InputDecoration(
                  errorText: isbnError,
                  labelText: 'ISBN',
                  hintText: 'Введите ISBN',
                  border: OutlineInputBorder(),
                ),
              ),

              const SizedBox(height: 24),

              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  ElevatedButton(
                    onPressed: () => Navigator.pop(context),
                    child: const Text("Отмена"),
                  ),
                  ElevatedButton(
                    onPressed: () {
                      final localTitleError = validator.validateTitle(
                        titleCtrl.text,
                      );
                      final localGenreError = validator.validateGenre(
                        genreCtrl.text,
                      );
                      final localPublicationYearError = validator
                          .validatePublicationYear(publicationYearCtrl.text);
                      final localIsbnError = validator.validateISBN(
                        isbnCtrl.text,
                      );

                      if (localTitleError != null ||
                          localGenreError != null ||
                          localPublicationYearError != null ||
                          localIsbnError != null) {
                        setState(() {
                          titleError = localTitleError;
                          genreError = localGenreError;
                          publicationYearError = localPublicationYearError;
                          isbnError = localIsbnError;
                        });
                        return;
                      }

                      final updatedBook = Book(
                        // Изменено с Dog на Book
                        id: book.id,
                        title: titleCtrl.text, // Изменено
                        publicationYear:
                            int.tryParse(publicationYearCtrl.text) ??
                            0, // Изменено
                        genre: genreCtrl.text, // Изменено
                        isbn: isbnCtrl.text, // Изменено
                      );

                      ref
                          .read(booksProvider.notifier)
                          .updateBook(
                            updatedBook,
                          ); // Изменено имя провайдера и метод

                      closeDialog();
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(
                          content: Text('Книга обновлена.'),
                        ), // Изменен текст
                      );
                    },
                    child: const Text('Сохранить'), // Изменен текст
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    ),
  );

  Future.delayed(const Duration(milliseconds: 200), () {
    if (context.mounted) {
      FocusScope.of(context).requestFocus(focusNode);
    }
  });
}
