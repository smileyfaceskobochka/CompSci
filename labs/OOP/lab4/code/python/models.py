from sqlalchemy import Column, Integer, String, BigInteger, ForeignKey
from sqlalchemy.orm import relationship, DeclarativeBase


class Base(DeclarativeBase):
    pass


class RacingSeries(Base):
    __tablename__ = "racing_series"

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    name = Column(String(255), nullable=False)
    official_name = Column(String(512))
    headquarters = Column(String(255), nullable=False)
    governing_body = Column(String(255))
    founded_year = Column(Integer)
    rounds = Column(Integer)

    teams = relationship("RacingTeam", back_populates="series", cascade="all, delete-orphan")

    def to_dict(self, include_teams=False):
        d = {
            "id": self.id,
            "name": self.name,
            "official_name": self.official_name,
            "headquarters": self.headquarters,
            "governing_body": self.governing_body,
            "founded_year": self.founded_year,
            "rounds": self.rounds,
        }
        if include_teams:
            d["teams"] = [t.to_dict() for t in self.teams]
        return d


class RacingTeam(Base):
    __tablename__ = "racing_teams"

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    name = Column(String(255), nullable=False)
    principal_name = Column(String(255), nullable=False)
    base_location = Column(String(255), nullable=False)
    power_unit = Column(String(255))
    points = Column(Integer, nullable=False, default=0)
    series_id = Column(BigInteger, ForeignKey("racing_series.id"), nullable=False)
    team_color = Column(String(10), default="#FFFFFF")

    series = relationship("RacingSeries", back_populates="teams")
    drivers = relationship("RacingDriver", back_populates="team", cascade="all, delete-orphan")

    def to_dict(self, include_series=False, include_drivers=False):
        d = {
            "id": self.id,
            "name": self.name,
            "principal_name": self.principal_name,
            "base_location": self.base_location,
            "power_unit": self.power_unit,
            "points": self.points,
            "series_id": self.series_id,
            "team_color": self.team_color,
        }
        if include_series and self.series:
            d["series"] = self.series.to_dict()
        if include_drivers:
            d["drivers"] = [dr.to_dict() for dr in self.drivers]
        return d


class RacingDriver(Base):
    __tablename__ = "racing_drivers"

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    first_name = Column(String(255), nullable=False)
    last_name = Column(String(255), nullable=False)
    racing_number = Column(Integer, nullable=False)
    points = Column(Integer, nullable=False, default=0)
    team_id = Column(BigInteger, ForeignKey("racing_teams.id"), nullable=False)

    team = relationship("RacingTeam", back_populates="drivers")

    def to_dict(self, include_team=False):
        d = {
            "id": self.id,
            "first_name": self.first_name,
            "last_name": self.last_name,
            "full_name": f"{self.first_name} {self.last_name}",
            "racing_number": self.racing_number,
            "points": self.points,
            "team_id": self.team_id,
        }
        if include_team and self.team:
            d["team"] = self.team.to_dict(include_series=True)
        return d
