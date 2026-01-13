import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../database.dart';
import '../providers.dart';
import '../validators/author_validator.dart';

void showAuthorEditingDialog(
  BuildContext context,
  Author author,
  WidgetRef ref,
) {
  final firstNameCtrl = TextEditingController(text: author.firstName);
  final lastNameCtrl = TextEditingController(text: author.lastName);
  final nationalityCtrl = TextEditingController(text: author.nationality);
  final birthYearCtrl = TextEditingController(text: '${author.birthYear}');

  Book? selectedBook;
  final booksList = ref.read(booksProvider);

  final focusNode = FocusNode();
  String? firstNameError;
  String? lastNameError;
  String? nationalityError;
  String? birthYearError;

  final AuthorValidator validator = AuthorValidator();

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
          child: SingleChildScrollView(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              children: [
                Text(
                  'Редактирование автора',
                  style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold),
                ),
                const SizedBox(height: 16),

                TextField(
                  controller: firstNameCtrl,
                  onChanged: (value) {
                    setState(() {
                      firstNameError = validator.validateFirstName(value);
                    });
                  },
                  focusNode: focusNode,
                  decoration: InputDecoration(
                    errorText: firstNameError,
                    labelText: 'Имя',
                    hintText: 'Введите имя автора',
                    border: OutlineInputBorder(),
                  ),
                ),

                const SizedBox(height: 12),

                TextField(
                  controller: lastNameCtrl,
                  onChanged: (value) {
                    setState(() {
                      lastNameError = validator.validateLastName(value);
                    });
                  },
                  decoration: InputDecoration(
                    errorText: lastNameError,
                    labelText: 'Фамилия',
                    hintText: 'Введите фамилию автора',
                    border: OutlineInputBorder(),
                  ),
                ),

                const SizedBox(height: 12),

                TextField(
                  controller: nationalityCtrl,
                  onChanged: (value) {
                    setState(() {
                      nationalityError = validator.validateNationality(value);
                    });
                  },
                  decoration: InputDecoration(
                    errorText: nationalityError,
                    labelText: 'Национальность',
                    hintText: 'Введите национальность',
                    border: OutlineInputBorder(),
                  ),
                ),

                const SizedBox(height: 12),

                TextField(
                  controller: birthYearCtrl,
                  keyboardType: TextInputType.number,
                  onChanged: (value) {
                    setState(() {
                      birthYearError = validator.validateBirthYear(value);
                    });
                  },
                  decoration: InputDecoration(
                    errorText: birthYearError,
                    labelText: 'Год рождения',
                    hintText: 'Введите год рождения',
                    border: OutlineInputBorder(),
                  ),
                ),

                const SizedBox(height: 12),

                booksList.when(
                  loading: () => const CircularProgressIndicator(),
                  error: (err, st) => Text('Ошибка загрузки книг: $err'),
                  data: (books) {
                    final List<DropdownMenuItem<Book?>> menuItems = [
                      const DropdownMenuItem<Book?>(
                        value: null,
                        child: Text('Нет (без книги)'),
                      ),
                    ];
                    menuItems.addAll(
                      books.map((book) {
                        return DropdownMenuItem<Book?>(
                          value: book,
                          child: Text('${book.title} (${book.genre})'),
                        );
                      }),
                    );
                    return DropdownButtonFormField<Book?>(
                      decoration: const InputDecoration(
                        labelText: 'Книга автора',
                      ),
                      initialValue: selectedBook,
                      items: menuItems,
                      onChanged: (value) =>
                          setState(() => selectedBook = value),
                    );
                  },
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
                        final localFirstNameError = validator.validateFirstName(
                          firstNameCtrl.text,
                        );
                        final localLastNameError = validator.validateLastName(
                          lastNameCtrl.text,
                        );
                        final localNationalityError = validator
                            .validateNationality(nationalityCtrl.text);
                        final localBirthYearError = validator.validateBirthYear(
                          birthYearCtrl.text,
                        );

                        if (localFirstNameError != null ||
                            localLastNameError != null ||
                            localNationalityError != null ||
                            localBirthYearError != null) {
                          setState(() {
                            firstNameError = localFirstNameError;
                            lastNameError = localLastNameError;
                            nationalityError = localNationalityError;
                            birthYearError = localBirthYearError;
                          });
                          return;
                        }

                        final updatedAuthor = Author(
                          id: author.id,
                          bookId: selectedBook?.id,
                          firstName: firstNameCtrl.text,
                          lastName: lastNameCtrl.text,
                          nationality: nationalityCtrl.text,
                          birthYear: int.tryParse(birthYearCtrl.text) ?? 0,
                        );

                        ref
                            .read(authorsProvider.notifier)
                            .updateAuthor(updatedAuthor);

                        closeDialog();
                        ScaffoldMessenger.of(context).showSnackBar(
                          const SnackBar(content: Text('Автор обновлен.')),
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
    ),
  );
}
