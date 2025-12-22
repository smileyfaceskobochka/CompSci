import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../providers.dart'; // Обновленный путь
import '../utils/confirm_delete_popup.dart'; // Обновленный путь
import '../widgets/book_redacting_popup.dart'; // Изменено

class BooksListPage extends ConsumerWidget {
  const BooksListPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final isFastDelete = ref.watch(
      fastDeleteProvider,
    ); // Изменено имя провайдера
    final booksAsyncValue = ref.watch(booksProvider); // Изменено имя провайдера

    return Scaffold(
      appBar: AppBar(
        centerTitle: true,
        title: const Text(
          'Список книг',
          textAlign: TextAlign.center,
        ), // Изменен заголовок
      ),
      body: booksAsyncValue.when(
        loading: CircularProgressIndicator.adaptive,
        data: (books) {
          if (books.isEmpty) {
            return const Center(child: Text('Книг нет!')); // Изменен текст
          }
          return ListView.builder(
            itemCount: books.length,
            itemBuilder: (context, index) {
              final book = books[index];
              return Card(
                margin: const EdgeInsets.symmetric(horizontal: 10, vertical: 4),
                child: ListTile(
                  leading: CircleAvatar(
                    // Небольшое изменение дизайна - круглая иконка с ID
                    child: Text(
                      '${book.id}',
                      style: const TextStyle(fontSize: 12),
                    ),
                  ),
                  title: Text(
                    '${book.title} (${book.genre})', // Обновлен вывод информации о книге
                    style: const TextStyle(fontWeight: FontWeight.bold),
                  ),
                  subtitle: Text(
                    'Год: ${book.publicationYear}; ISBN: ${book.isbn}', // Обновлен вывод информации о книге
                  ),
                  trailing: Row(
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      IconButton(
                        onPressed: () => showBookEditingDialog(
                          context,
                          book,
                          ref,
                        ), // Изменено название функции
                        icon: const Icon(Icons.edit, color: Colors.blue),
                      ),
                      IconButton(
                        onPressed: () => showDeleteConfirmation(
                          context: context,
                          itemName: book.title, // Используем название книги
                          title: 'Удалить книгу?', // Изменен заголовок
                          isFastDelete: isFastDelete,
                          onDelete: () async {
                            await ref
                                .read(booksProvider.notifier)
                                .deleteBook(book.id); // Изменено имя провайдера
                          },
                        ),
                        icon: const Icon(Icons.delete, color: Colors.red),
                      ),
                    ],
                  ),
                ),
              );
            },
          );
        },
        error: (err, stack) => Center(child: Text('Ошибка: $err')),
      ),
      floatingActionButton: Padding(
        padding: const EdgeInsets.only(
          bottom: 50.0,
        ), // Отступ от нижней навигации
        child: Column(
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.end,
          children: [
            Container(
              padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
              decoration: BoxDecoration(
                color: Theme.of(context).cardColor,
                borderRadius: BorderRadius.circular(10),
                boxShadow: [
                  BoxShadow(
                    color: Colors.grey.withValues(alpha: 51),
                    spreadRadius: 1,
                    blurRadius: 3,
                    offset: const Offset(0, 2),
                  ),
                ],
              ),
              child: Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  const Text('Быстрое удаление'),
                  Switch(
                    value: isFastDelete,
                    onChanged: (newValue) {
                      ref.read(fastDeleteProvider.notifier).state =
                          newValue; // Изменено имя провайдера
                    },
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
      floatingActionButtonLocation:
          FloatingActionButtonLocation.endDocked, // Расположение FAB
    );
  }
}
