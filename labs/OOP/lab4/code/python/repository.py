from sqlalchemy.orm import Session
from sqlalchemy import func
from models import RacingSeries, RacingTeam, RacingDriver


# ─────────────────────────────────────────────────────────────────────────────
# Repository base
# ─────────────────────────────────────────────────────────────────────────────
class BaseRepository:
    def __init__(self, session: Session):
        self.session = session


# ─────────────────────────────────────────────────────────────────────────────
# Series repository
# ─────────────────────────────────────────────────────────────────────────────
class SeriesRepository(BaseRepository):
    def get_all(self) -> list[RacingSeries]:
        return self.session.query(RacingSeries).order_by(RacingSeries.id).all()

    def get_by_id(self, series_id: int) -> RacingSeries | None:
        return self.session.get(RacingSeries, series_id)

    def create(self, data: dict) -> RacingSeries:
        series = RacingSeries(**data)
        self.session.add(series)
        self.session.commit()
        self.session.refresh(series)
        return series

    def update(self, series_id: int, data: dict) -> RacingSeries | None:
        series = self.get_by_id(series_id)
        if not series:
            return None
        for key, value in data.items():
            setattr(series, key, value)
        self.session.commit()
        self.session.refresh(series)
        return series

    def delete(self, series_id: int) -> bool:
        series = self.get_by_id(series_id)
        if not series:
            return False
        self.session.delete(series)
        self.session.commit()
        return True


# ─────────────────────────────────────────────────────────────────────────────
# Team repository
# ─────────────────────────────────────────────────────────────────────────────
class TeamRepository(BaseRepository):
    def get_all(self) -> list[RacingTeam]:
        return self.session.query(RacingTeam).order_by(RacingTeam.points.desc()).all()

    def get_by_id(self, team_id: int) -> RacingTeam | None:
        return self.session.get(RacingTeam, team_id)

    def get_by_series(self, series_id: int) -> list[RacingTeam]:
        return (
            self.session.query(RacingTeam)
            .filter(RacingTeam.series_id == series_id)
            .order_by(RacingTeam.points.desc())
            .all()
        )

    def create(self, data: dict) -> RacingTeam:
        team = RacingTeam(**data)
        self.session.add(team)
        self.session.commit()
        self.session.refresh(team)
        return team

    def update(self, team_id: int, data: dict) -> RacingTeam | None:
        team = self.get_by_id(team_id)
        if not team:
            return None
        for key, value in data.items():
            setattr(team, key, value)
        self.session.commit()
        self.session.refresh(team)
        return team

    def delete(self, team_id: int) -> bool:
        team = self.get_by_id(team_id)
        if not team:
            return False
        self.session.delete(team)
        self.session.commit()
        return True

    # ── Business helpers ──────────────────────────────────────────────────────
    def avg_points_by_series(self) -> list[dict]:
        rows = (
            self.session.query(
                RacingSeries.name,
                func.avg(RacingTeam.points).label("avg_pts"),
                func.count(RacingTeam.id).label("team_count"),
            )
            .join(RacingSeries, RacingTeam.series_id == RacingSeries.id)
            .group_by(RacingSeries.name)
            .all()
        )
        return [{"series": r.name, "avg_points": round(float(r.avg_pts), 1), "team_count": r.team_count} for r in rows]


# ─────────────────────────────────────────────────────────────────────────────
# Driver repository
# ─────────────────────────────────────────────────────────────────────────────
class DriverRepository(BaseRepository):
    def get_all(self) -> list[RacingDriver]:
        return self.session.query(RacingDriver).order_by(RacingDriver.points.desc()).all()

    def get_by_id(self, driver_id: int) -> RacingDriver | None:
        return self.session.get(RacingDriver, driver_id)

    def get_by_team(self, team_id: int) -> list[RacingDriver]:
        return (
            self.session.query(RacingDriver)
            .filter(RacingDriver.team_id == team_id)
            .order_by(RacingDriver.points.desc())
            .all()
        )

    def get_standings_by_series(self, series_id: int) -> list[RacingDriver]:
        return (
            self.session.query(RacingDriver)
            .join(RacingTeam, RacingDriver.team_id == RacingTeam.id)
            .filter(RacingTeam.series_id == series_id)
            .order_by(RacingDriver.points.desc())
            .all()
        )

    def create(self, data: dict) -> RacingDriver:
        driver = RacingDriver(**data)
        self.session.add(driver)
        self.session.commit()
        self.session.refresh(driver)
        return driver

    def update(self, driver_id: int, data: dict) -> RacingDriver | None:
        driver = self.get_by_id(driver_id)
        if not driver:
            return None
        for key, value in data.items():
            setattr(driver, key, value)
        self.session.commit()
        self.session.refresh(driver)
        return driver

    def delete(self, driver_id: int) -> bool:
        driver = self.get_by_id(driver_id)
        if not driver:
            return False
        self.session.delete(driver)
        self.session.commit()
        return True

    def total_points_by_series(self) -> list[dict]:
        rows = (
            self.session.query(
                RacingSeries.name,
                func.sum(RacingDriver.points).label("total_pts"),
                func.count(RacingDriver.id).label("driver_count"),
            )
            .join(RacingTeam, RacingDriver.team_id == RacingTeam.id)
            .join(RacingSeries, RacingTeam.series_id == RacingSeries.id)
            .group_by(RacingSeries.name)
            .all()
        )
        return [{"series": r.name, "total_points": int(r.total_pts or 0), "driver_count": r.driver_count} for r in rows]
