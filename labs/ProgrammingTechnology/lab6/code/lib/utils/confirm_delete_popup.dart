import 'package:flutter/material.dart';

void showDeleteConfirmation({
  required BuildContext context,
  required bool isFastDelete,
  required String title,
  required String itemName,
  required Future<void> Function() onDelete,
}) {
  Future<void> performDelete() async {
    try {
      await onDelete();

      if (context.mounted) {
        if (!isFastDelete) {
          Navigator.of(context).pop();
        }

        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('$itemName успешно удален(а)'), // Обновлен текст
            action: SnackBarAction(label: 'ОК', onPressed: () {}),
          ),
        );
      }
    } catch (e) {
      if (context.mounted) {
        if (!isFastDelete) {
          Navigator.of(context).pop();
        }

        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Ошибка удаления: $e'),
            backgroundColor: Colors.red,
          ),
        );
      }
    }
  }

  if (isFastDelete) {
    performDelete();
  } else {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text(title, textAlign: TextAlign.center),
        content: Text(
          'Вы действительно хотите удалить "$itemName"? Это действие нельзя отменить.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text("Отмена"),
          ),
          TextButton(
            onPressed: () {
              performDelete();
              // Закрыть диалог после вызова performDelete, чтобы избежать двойного удаления
              // если performDelete делает что-то асинхронное и потом обновляет UI
              // Здесь это уже учтено: performDelete может закрыть, но только если !isFastDelete
              if (context.mounted) Navigator.pop(context);
            },
            child: const Text("Удалить", style: TextStyle(color: Colors.red)),
          ),
        ],
      ),
    );
  }
}