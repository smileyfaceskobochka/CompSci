import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Base, RacingSeries, RacingTeam, RacingDriver
from repository import SeriesRepository, TeamRepository, DriverRepository


@pytest.fixture
def db_session():
    engine = create_engine("sqlite:///:memory:", echo=False)
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()
    yield session
    session.close()


@pytest.fixture
def seeded_session(db_session):
    s1 = RacingSeries(id=1, name="Formula 1", headquarters="London", governing_body="FIA", founded_year=1950, rounds=24)
    s2 = RacingSeries(id=2, name="WEC", headquarters="Paris", governing_body="ACO", founded_year=2012, rounds=8)
    db_session.add_all([s1, s2])
    db_session.commit()

    t1 = RacingTeam(id=1, name="Red Bull", principal_name="Christian Horner", base_location="Milton Keynes",
                    power_unit="Honda RBPT", points=200, series_id=s1.id, team_color="#1e41ff")
    t2 = RacingTeam(id=2, name="Ferrari", principal_name="Frederic Vasseur", base_location="Maranello",
                    power_unit="Ferrari", points=180, series_id=s1.id, team_color="#dc0000")
    db_session.add_all([t1, t2])
    db_session.commit()

    d1 = RacingDriver(id=1, first_name="Max", last_name="Verstappen", racing_number=1, points=150, team_id=t1.id)
    d2 = RacingDriver(id=2, first_name="Charles", last_name="Leclerc", racing_number=16, points=120, team_id=t2.id)
    db_session.add_all([d1, d2])
    db_session.commit()
    return db_session


class TestSeriesRepository:
    def test_get_all(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        all_ = repo.get_all()
        assert len(all_) == 2

    def test_get_by_id(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        s = repo.get_by_id(1)
        assert s is not None
        assert s.name == "Formula 1"

    def test_get_by_id_not_found(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        assert repo.get_by_id(999) is None

    def test_create(self, db_session):
        repo = SeriesRepository(db_session)
        s = repo.create({"id": 10, "name": "IndyCar", "headquarters": "Indianapolis", "rounds": 17})
        assert s.id is not None
        assert s.name == "IndyCar"

    def test_update(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        s = repo.update(1, {"rounds": 25})
        assert s is not None
        assert s.rounds == 25

    def test_update_not_found(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        assert repo.update(999, {"name": "X"}) is None

    def test_delete(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        assert repo.delete(2) is True
        assert repo.get_by_id(2) is None

    def test_delete_not_found(self, seeded_session):
        repo = SeriesRepository(seeded_session)
        assert repo.delete(999) is False


class TestTeamRepository:
    def test_get_all(self, seeded_session):
        repo = TeamRepository(seeded_session)
        all_ = repo.get_all()
        assert len(all_) == 2

    def test_get_by_series(self, seeded_session):
        repo = TeamRepository(seeded_session)
        teams = repo.get_by_series(1)
        assert len(teams) == 2

    def test_create(self, db_session):
        s = RacingSeries(id=1, name="F1", headquarters="L", rounds=20)
        db_session.add(s)
        db_session.commit()
        repo = TeamRepository(db_session)
        t = repo.create({"id": 10, "name": "New Team", "principal_name": "Boss", "base_location": "Loc",
                         "points": 50, "series_id": s.id})
        assert t.id is not None
        assert t.name == "New Team"

    def test_update(self, seeded_session):
        repo = TeamRepository(seeded_session)
        t = repo.update(1, {"points": 999})
        assert t.points == 999

    def test_delete(self, seeded_session):
        repo = TeamRepository(seeded_session)
        assert repo.delete(1) is True
        assert repo.get_by_id(1) is None

    def test_avg_points_by_series(self, seeded_session):
        repo = TeamRepository(seeded_session)
        stats = repo.avg_points_by_series()
        assert len(stats) > 0
        f1 = [s for s in stats if s["series"] == "Formula 1"]
        assert len(f1) == 1
        assert f1[0]["avg_points"] == 190.0


class TestDriverRepository:
    def test_get_all(self, seeded_session):
        repo = DriverRepository(seeded_session)
        all_ = repo.get_all()
        assert len(all_) == 2

    def test_get_by_team(self, seeded_session):
        repo = DriverRepository(seeded_session)
        drivers = repo.get_by_team(1)
        assert len(drivers) == 1
        assert drivers[0].first_name == "Max"

    def test_get_standings_by_series(self, seeded_session):
        repo = DriverRepository(seeded_session)
        drivers = repo.get_standings_by_series(1)
        assert len(drivers) == 2
        assert drivers[0].points >= drivers[1].points

    def test_create(self, db_session):
        s = RacingSeries(id=1, name="F1", headquarters="L", rounds=20)
        db_session.add(s)
        db_session.commit()
        t = RacingTeam(id=1, name="T", principal_name="P", base_location="L", series_id=s.id)
        db_session.add(t)
        db_session.commit()
        repo = DriverRepository(db_session)
        d = repo.create({"id": 10, "first_name": "New", "last_name": "Driver", "racing_number": 99,
                         "points": 10, "team_id": t.id})
        assert d.id is not None
        assert d.first_name == "New"

    def test_total_points_by_series(self, seeded_session):
        repo = DriverRepository(seeded_session)
        stats = repo.total_points_by_series()
        assert len(stats) > 0
        f1 = [s for s in stats if s["series"] == "Formula 1"]
        assert len(f1) == 1
        assert f1[0]["total_points"] == 270
