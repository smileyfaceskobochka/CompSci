import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../database.dart';
import '../providers.dart';
import '../validators/book_validator.dart';

void showBookEditingDialog(BuildContext context, Book book, WidgetRef ref) {
  final titleCtrl = TextEditingController(text: book.title);
  final genreCtrl = TextEditingController(text: book.genre);
  final publicationYearCtrl = TextEditingController(
    text: '${book.publicationYear}',
  );
  final isbnCtrl = TextEditingController(text: book.isbn);

  final focusNode = FocusNode();
  String? titleError;
  String? genreError;
  String? publicationYearError;
  String? isbnError;

  final BookValidator validator = BookValidator();

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
                'Редактирование книги',
                style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
              ),
              const SizedBox(height: 16),

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
                        id: book.id,
                        title: titleCtrl.text,
                        publicationYear:
                            int.tryParse(publicationYearCtrl.text) ?? 0,
                        genre: genreCtrl.text,
                        isbn: isbnCtrl.text,
                      );

                      ref.read(booksProvider.notifier).updateBook(updatedBook);

                      closeDialog();
                      ScaffoldMessenger.of(context).showSnackBar(
                        const SnackBar(content: Text('Книга обновлена.')),
                      );
                    },
                    child: const Text('Сохранить'),
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    ),
  );
}
