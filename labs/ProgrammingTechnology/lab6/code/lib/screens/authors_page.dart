import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../providers.dart'; // Обновленный путь
import '../utils/confirm_delete_popup.dart'; // Обновленный путь

class AuthorsPage extends ConsumerWidget {
  const AuthorsPage({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final isFastDelete = ref.watch(
      fastDeleteProvider,
    ); // Изменено имя провайдера
    final authorsAsyncValue = ref.watch(
      authorsProvider,
    ); // Изменено имя провайдера

    return Scaffold(
      appBar: AppBar(
        centerTitle: true,
        title: const Text('Список авторов'),
      ), // Изменен заголовок
      body: authorsAsyncValue.when(
        loading: () => const Center(child: CircularProgressIndicator()),
        data: (authors) {
          if (authors.isEmpty) {
            return const Center(child: Text('Авторов нет!')); // Изменен текст
          }
          return ListView.builder(
            itemCount: authors.length,
            itemBuilder: (context, index) {
              final author = authors[index];
              return Card(
                margin: const EdgeInsets.symmetric(horizontal: 10, vertical: 4),
                child: ListTile(
                  leading: CircleAvatar(
                    // Небольшое изменение дизайна - круглая иконка с ID
                    child: Text(
                      '${author.id}',
                      style: const TextStyle(fontSize: 12),
                    ),
                  ),
                  title: Text(
                    '${author.lastName} ${author.firstName}', // Обновлен вывод информации об авторе
                    style: const TextStyle(fontWeight: FontWeight.bold),
                  ),
                  subtitle: Text(
                    'Национальность: ${author.nationality}\nГод рождения: ${author.birthYear}\nID книги: ${author.bookId ?? 'не указан'}', // Обновлен вывод информации
                  ),
                  trailing: Row(
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      // Кнопка редактирования автора (реализация не предусмотрена в оригинале, но добавлена как заглушка)
                      IconButton(
                        onPressed: () {
                          // TODO: Реализовать диалог редактирования автора
                          ScaffoldMessenger.of(context).showSnackBar(
                            const SnackBar(
                              content: Text(
                                'Редактирование автора пока не реализовано',
                              ),
                            ),
                          );
                        },
                        icon: const Icon(
                          Icons.edit,
                          color: Colors.grey,
                        ), // Серая иконка для неактивной функции
                      ),
                      IconButton(
                        onPressed: () async {
                          showDeleteConfirmation(
                            context: context,
                            isFastDelete: isFastDelete,
                            title: 'Удалить автора?', // Изменен заголовок
                            itemName:
                                '${author.firstName} ${author.lastName}', // Имя автора
                            onDelete: () async {
                              await ref
                                  .read(authorsProvider.notifier)
                                  .deleteAuthor(
                                    author.id,
                                  ); // Изменено имя провайдера
                            },
                          );
                        },
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
