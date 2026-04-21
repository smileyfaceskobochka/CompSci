#ifndef DRIVER_H
#define DRIVER_H

#include <string>

class Driver {
private:
    std::string firstName;
    std::string lastName;
    int racingNumber;
    int points;

public:
    Driver(const std::string& fName, const std::string& lName, int number, int pts = 0);
    
    // Getters
    std::string getFirstName() const;
    std::string getLastName() const;
    std::string getFullName() const;
    int getRacingNumber() const;
    int getPoints() const;

    // Modifiers
    void addPoints(int pts);
};

#endif
