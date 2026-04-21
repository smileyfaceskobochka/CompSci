#include "Driver.h"

Driver::Driver(const std::string& fName, const std::string& lName, int number, int pts)
    : firstName(fName), lastName(lName), racingNumber(number), points(pts) {}

std::string Driver::getFirstName() const {
    return firstName;
}

std::string Driver::getLastName() const {
    return lastName;
}

std::string Driver::getFullName() const {
    return firstName + " " + lastName;
}

int Driver::getRacingNumber() const {
    return racingNumber;
}

int Driver::getPoints() const {
    return points;
}

void Driver::addPoints(int pts) {
    points += pts;
}

