import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../widgets/book_adding_window.dart';
import '../widgets/author_adding_window.dart';

class HomePage extends ConsumerStatefulWidget {
  const HomePage({super.key});

  @override
  ConsumerState<HomePage> createState() => _HomePageState();
}

class _HomePageState extends ConsumerState<HomePage> {
  String? _appBarTitle = 'Добавить книгу';
  int _showWindow = 0;

  Widget _buildWindow() {
    switch (_showWindow) {
      case 0:
        return const BookAddingWindow();
      case 1:
        return const AuthorAddingWindow();
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
          Expanded(child: _buildWindow()),
          Container(
            padding: const EdgeInsets.only(bottom: 16, top: 16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                const Text(
                  'Что добавить:',
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
                          _appBarTitle = 'Добавить книгу';
                        });
                      },
                      child: const Text('Книгу'),
                    ),
                    const SizedBox(width: 16),
                    TextButton(
                      onPressed: () {
                        setState(() {
                          _showWindow = 1;
                          _appBarTitle = 'Добавить автора';
                        });
                      },
                      child: const Text('Автора'),
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
