class BookValidator {
  String? validateTitle(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите название книги';
    }
    return null;
  }

  String? validateGenre(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите жанр';
    }
    return null;
  }

  String? validatePublicationYear(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите год публикации';
    }

    final year = int.tryParse(value);
    if (year == null) {
      return 'Введите целое число';
    }

    if (year < 0 || year > DateTime.now().year) {
      return 'Введите действительный год публикации';
    }

    return null;
  }

  String? validateISBN(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите ISBN';
    }
    // Простая валидация ISBN (можно усложнить)
    final isbnExp = RegExp(r'^(?:ISBN(?::?)(?=\s*\d{3}[\s-]?\d{10}$))?((?:97[89][\s-]?)?\d{1,5}[\s-]?\d{1,7}[\s-]?\d{1,6}[\s-]?\d)$');
    if (!isbnExp.hasMatch(value)) {
      return 'Некорректный ISBN';
    }
    return null;
  }
}