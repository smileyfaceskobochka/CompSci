import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'database.dart'; // Обновленный путь
import 'package:riverpod/legacy.dart';

// Notifier для управления состоянием списка книг
class BookNotifier extends AsyncNotifier<List<Book>> {
  @override
  Future<List<Book>> build() async {
    final list = await DatabaseHelper().getAll("books");
    return list.map((e) => Book.fromMap(e)).toList();
  }

  Future<void> addBook(Book book) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().insert("books", book.toMap());
      return await _fetchBooks(); // Обновление списка
    });
  }

  Future<void> deleteBook(int id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().delete("books", id);
      return await _fetchBooks(); // Обновление списка
    });
  }

  Future<void> updateBook(Book book) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().update("books", book.id, book.toMap());
      return await _fetchBooks(); // Обновление списка
    });
  }

  Future<List<Book>> _fetchBooks() async {
    final list = await DatabaseHelper().getAll("books");
    return list.map((e) => Book.fromMap(e)).toList();
  }
}

final booksProvider = AsyncNotifierProvider<BookNotifier, List<Book>>(
  () => BookNotifier(),
);

// Notifier для управления состоянием списка авторов
class AuthorNotifier extends AsyncNotifier<List<Author>> {
  @override
  Future<List<Author>> build() async {
    final list = await DatabaseHelper().getAll("authors");
    return list.map((e) => Author.fromMap(e)).toList();
  }

  Future<void> addAuthor(Author author) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().insert("authors", author.toMap());
      return await _fetchAuthors(); // Обновление списка
    });
  }

  Future<void> updateAuthor(Author author) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().update("authors", author.id, author.toMap());
      return await _fetchAuthors(); // Обновление списка
    });
  }

  Future<void> deleteAuthor(int id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() async {
      await DatabaseHelper().delete("authors", id);
      return await _fetchAuthors(); // Обновление списка
    });
  }

  Future<List<Author>> _fetchAuthors() async {
    final list = await DatabaseHelper().getAll("authors");
    return list.map((e) => Author.fromMap(e)).toList();
  }
}

final authorsProvider = AsyncNotifierProvider<AuthorNotifier, List<Author>>(
  () => AuthorNotifier(),
);

// Провайдер для быстрого удаления
final fastDeleteProvider = StateProvider<bool>((ref) => false);
