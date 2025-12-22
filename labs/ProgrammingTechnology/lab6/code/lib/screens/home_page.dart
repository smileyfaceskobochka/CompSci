import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../widgets/book_adding_window.dart'; // Изменено
import '../widgets/author_adding_window.dart'; // Изменено

class HomePage extends ConsumerStatefulWidget {
  const HomePage({super.key});

  @override
  ConsumerState<HomePage> createState() => _HomePageState();
}

class _HomePageState extends ConsumerState<HomePage> {
  String? _appBarTitle = 'Добавить книгу'; // Изменено
  int _showWindow = 0; // Изменено имя переменной

  Widget _buildWindow() {
    switch (_showWindow) { // Изменено имя переменной
      case 0:
        return const BookAddingWindow(); // Изменено
      case 1:
        return const AuthorAddingWindow(); // Изменено
      default:
        return const SizedBox();
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(centerTitle: true, title: Text('$_appBarTitle')),
      body: Column(
        children: [
          Expanded(child: _buildWindow()), // Wrap with Expanded
          Container(
            padding: const EdgeInsets.only(bottom: 16, top: 16), // Добавлен top padding
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                const Text(
                  'Что добавить:', // Изменен текст
                  style: TextStyle(fontSize: 16, fontWeight: FontWeight.w500),
                ),
                const SizedBox(height: 8),
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                  crossAxisAlignment: CrossAxisAlignment.center,
                  children: [
                    TextButton(
                      onPressed: () {
                        setState(() {
                          _showWindow = 0;
                          _appBarTitle = 'Добавить книгу'; // Изменено
                        });
                      },
                      child: const Text('Книгу'), // Изменено
                    ),
                    const SizedBox(width: 16),
                    TextButton(
                      onPressed: () {
                        setState(() {
                          _showWindow = 1;
                          _appBarTitle = 'Добавить автора'; // Изменено
                        });
                      },
                      child: const Text('Автора'), // Изменено
                    ),
                  ],
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
}