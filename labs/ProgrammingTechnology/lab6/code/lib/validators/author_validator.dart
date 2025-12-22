class AuthorValidator {
  String? validateFirstName(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите имя автора";
    }
    if (value.length < 2) {
      return "Имя слишком короткое";
    }
    return null;
  }

  String? validateLastName(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите фамилию автора";
    }
    return null;
  }

  String? validateNationality(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите национальность";
    }
    return null;
  }

  String? validateBirthYear(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите год рождения';
    }

    final year = int.tryParse(value);
    if (year == null) {
      return 'Введите целое число';
    }

    if (year < 0 || year > DateTime.now().year) {
      return 'Введите действительный год рождения';
    }

    return null;
  }
}