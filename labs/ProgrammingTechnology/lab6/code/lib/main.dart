import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'screens/books_page.dart';
import 'screens/authors_page.dart';
import 'screens/home_page.dart';

void main() {
  runApp(const ProviderScope(child: MyApp()));
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    final lightColorScheme = ColorScheme.fromSeed(
      seedColor: const Color.fromARGB(255, 68, 138, 201),
      brightness: Brightness.light,
    );

    final darkColorScheme = ColorScheme.fromSeed(
      seedColor: const Color.fromARGB(255, 68, 138, 201),
      brightness: Brightness.dark,
    );

    return MaterialApp(
      themeMode: ThemeMode.system,
      title: 'Моя Библиотека',
      darkTheme: ThemeData(colorScheme: darkColorScheme, useMaterial3: true),
      theme: ThemeData(
        colorScheme: lightColorScheme,
        useMaterial3: true,
        appBarTheme: AppBarTheme(
          backgroundColor: lightColorScheme.primary,
          foregroundColor: lightColorScheme.onPrimary,
        ),
        inputDecorationTheme: InputDecorationTheme(
          labelStyle: TextStyle(
            fontSize: 18,
            color: lightColorScheme.onSurfaceVariant,
          ),
          border: const OutlineInputBorder(
            borderRadius: BorderRadius.all(Radius.circular(8)),
          ),
          focusedBorder: OutlineInputBorder(
            borderSide: BorderSide(color: lightColorScheme.primary, width: 2),
            borderRadius: const BorderRadius.all(Radius.circular(8)),
          ),
          errorBorder: OutlineInputBorder(
            borderSide: BorderSide(color: lightColorScheme.error, width: 2),
            borderRadius: const BorderRadius.all(Radius.circular(8)),
          ),
          focusedErrorBorder: OutlineInputBorder(
            borderSide: BorderSide(color: lightColorScheme.error, width: 2),
            borderRadius: const BorderRadius.all(Radius.circular(8)),
          ),
          enabledBorder: OutlineInputBorder(
            borderSide: BorderSide(color: lightColorScheme.outline),
            borderRadius: const BorderRadius.all(Radius.circular(8)),
          ),
          floatingLabelBehavior: FloatingLabelBehavior.always,
        ),
        cardTheme: CardThemeData(
          elevation: 4,
          margin: const EdgeInsets.symmetric(horizontal: 10, vertical: 5),
          color: lightColorScheme.surface,
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(12),
          ),
        ),
        navigationBarTheme: NavigationBarThemeData(
          indicatorColor: lightColorScheme.primaryContainer,
          labelTextStyle: WidgetStateProperty.resolveWith((states) {
            if (states.contains(WidgetState.selected)) {
              return TextStyle(
                color: lightColorScheme.onSurface,
                fontWeight: FontWeight.bold,
              );
            }
            return TextStyle(color: lightColorScheme.onSurfaceVariant);
          }),
        ),
      ),
      home: const MainScreen(title: 'Моя Библиотека'),
    );
  }
}

class MainScreen extends StatefulWidget {
  const MainScreen({super.key, required this.title});

  final String title;

  @override
  State<MainScreen> createState() => _MainScreenState();
}

class _MainScreenState extends State<MainScreen> {
  final List<Widget> _screens = const [
    HomePage(),
    BooksListPage(),
    AuthorsPage(),
  ];
  int _currentIndex = 0;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.title), centerTitle: true),
      body: IndexedStack(index: _currentIndex, children: _screens),
      bottomNavigationBar: NavigationBar(
        selectedIndex: _currentIndex,
        onDestinationSelected: (index) {
          setState(() => _currentIndex = index);
        },
        destinations: const [
          NavigationDestination(
            icon: Icon(Icons.add_circle_outline),
            label: 'Добавить',
          ),
          NavigationDestination(icon: Icon(Icons.book), label: 'Книги'),
          NavigationDestination(icon: Icon(Icons.person), label: 'Авторы'),
        ],
      ),
    );
  }
}
