import 'package:sqflite/sqflite.dart';
import 'package:path/path.dart';

class DatabaseHelper {
  static final DatabaseHelper _instance = DatabaseHelper._internal();
  factory DatabaseHelper() => _instance;
  DatabaseHelper._internal();

  static Database? _database;

  Future<Database> get database async {
    if (_database != null) return _database!;
    _database = await _initDatabase();
    return _database!;
  }

  Future<Database> _initDatabase() async {
    final dbPath = await getDatabasesPath();
    final path = join(dbPath, 'bookstore_database_v1.db'); // Изменено имя базы данных

    return await openDatabase(
      path,
      version: 1,
      onConfigure: (db) async {
        await db.execute('PRAGMA foreign_keys = ON');
      },
      onCreate: (db, version) async {
        // Таблица для книг
        await db.execute('''
          CREATE TABLE books (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            title TEXT NOT NULL,
            publication_year INTEGER NOT NULL,
            genre TEXT NOT NULL,
            isbn TEXT NOT NULL
          );
        ''');

        // Таблица для авторов
        await db.execute('''
          CREATE TABLE authors (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            book_id INTEGER,
            first_name TEXT NOT NULL,
            last_name TEXT NOT NULL,
            nationality TEXT NOT NULL,
            birth_year INTEGER NOT NULL,
            FOREIGN KEY (book_id) REFERENCES books (id) ON DELETE SET NULL
          );
        ''');
      },
    );
  }

  Future<int> insert(String table, Map<String, dynamic> data) async {
    final db = await database;
    return await db.insert(
      table,
      data,
      conflictAlgorithm: ConflictAlgorithm.replace,
    );
  }

  Future<List<Map<String, dynamic>>> getAll(String table) async {
    final db = await database;
    return await db.query(table);
  }

  Future<Map<String, dynamic>?> getById(String table, int id) async {
    final db = await database;
    final res = await db.query(
      table,
      where: 'id = ?',
      whereArgs: [id],
      limit: 1,
    );
    return res.isNotEmpty ? res.first : null;
  }

  Future<int> update(String table, int id, Map<String, dynamic> data) async {
    final db = await database;
    return await db.update(table, data, where: 'id = ?', whereArgs: [id]);
  }

  Future<int> delete(String table, int id) async {
    final db = await database;
    return await db.delete(table, where: 'id = ?', whereArgs: [id]);
  }
}

// Модель данных для Книги
class Book {
  final int id;
  final String title;
  final int publicationYear;
  final String genre;
  final String isbn;

  Book({
    this.id = 0,
    required this.title,
    required this.publicationYear,
    required this.genre,
    required this.isbn,
  });

  Map<String, dynamic> toMap() {
    return {
      if (id != 0) 'id': id,
      'title': title,
      'publication_year': publicationYear,
      'genre': genre,
      'isbn': isbn,
    };
  }

  factory Book.fromMap(Map<String, dynamic> map) {
    return Book(
      id: map['id'],
      title: map['title'],
      publicationYear: map['publication_year'],
      genre: map['genre'],
      isbn: map['isbn'],
    );
  }
}

// Модель данных для Автора
class Author {
  final int id;
  final int? bookId; // Связь с книгой
  final String firstName;
  final String lastName;
  final String nationality;
  final int birthYear;

  Author({
    this.id = 0,
    this.bookId,
    required this.firstName,
    required this.lastName,
    required this.nationality,
    required this.birthYear,
  });

  Map<String, dynamic> toMap() {
    return {
      if (id != 0) 'id': id,
      'book_id': bookId,
      'first_name': firstName,
      'last_name': lastName,
      'nationality': nationality,
      'birth_year': birthYear,
    };
  }

  factory Author.fromMap(Map<String, dynamic> map) {
    return Author(
      id: map['id'],
      bookId: map['book_id'],
      firstName: map['first_name'],
      lastName: map['last_name'],
      nationality: map['nationality'],
      birthYear: map['birth_year'],
    );
  }
}