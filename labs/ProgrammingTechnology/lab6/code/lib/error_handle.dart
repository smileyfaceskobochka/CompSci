class DogValidator {
  String? validateBreed(String? value) {
    if (value == null || value.isEmpty) {
      return 'respect';
    }
    return null;
  }

  String? validateName(String? value) {
    if (value == null || value.isEmpty) {
      return 'Введите имя собаки';
    }
    return null;
  }

  String? validateAge(String? value) {
    if (value == null || value.isEmpty) {
      return 'Собака существует?';
    }

    final age = int.tryParse(value);
    if (age == null) {
      return 'Введите целое число';
    }

    if (age > 115) {
      return 'Введите действительный возраст';
    }

    return null;
  }
}

class OwnerValidator {
  String? validateFirstName(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите имя";
    }
    if (value.length < 2) {
      return "Имя слишком короткое";
    }
    return null;
  }

  String? validateMiddleName(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите фамилию";
    }
    return null;
  }

  String? validateLastName(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите отчество";
    }
    return null;
  }

  String? validateAddress(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите адрес";
    }
    return null;
  }

  String? validatePhone(String? value) {
    if (value == null || value.trim().isEmpty) {
      return "Введите номер телефона";
    }

    final phoneExp = RegExp(r'^[0-9+\-\s]{6,18}$');
    if (!phoneExp.hasMatch(value)) {
      return "Некорректный номер телефона";
    }
    return null;
  }
}
