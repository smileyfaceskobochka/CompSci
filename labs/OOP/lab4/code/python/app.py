import os
from flask import Flask, request, jsonify, abort
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, Session

from models import Base, RacingSeries, RacingTeam, RacingDriver
from repository import SeriesRepository, TeamRepository, DriverRepository

# ─────────────────────────────────────────────────────────────────────────────
DB_URL = os.getenv(
    "DATABASE_URL",
    "postgresql://postgres:postgres@localhost:5432/postgres",
)

engine = create_engine(DB_URL, echo=False)
SessionFactory = sessionmaker(bind=engine)

app = Flask(__name__)


def get_db() -> Session:
    return SessionFactory()


@app.get("/api/series")
def list_series():
    db = get_db()
    repo = SeriesRepository(db)
    data = [s.to_dict() for s in repo.get_all()]
    db.close()
    return jsonify(data)


@app.get("/api/series/<int:sid>")
def get_series(sid):
    db = get_db()
    s = SeriesRepository(db).get_by_id(sid)
    if not s:
        db.close()
        abort(404, description="Series not found")
    data = s.to_dict(include_teams=True)
    db.close()
    return jsonify(data)


@app.post("/api/series")
def create_series():
    data = request.get_json(force=True)
    db = get_db()
    s = SeriesRepository(db).create(data)
    db.close()
    return jsonify(s.to_dict()), 201


@app.put("/api/series/<int:sid>")
def update_series(sid):
    data = request.get_json(force=True)
    db = get_db()
    s = SeriesRepository(db).update(sid, data)
    db.close()
    if not s:
        abort(404)
    return jsonify(s.to_dict())


@app.delete("/api/series/<int:sid>")
def delete_series(sid):
    db = get_db()
    ok = SeriesRepository(db).delete(sid)
    db.close()
    if not ok:
        abort(404)
    return jsonify({"deleted": sid})


# ─────────────────────────────────────────────────────────────────────────────
# TEAMS
# ─────────────────────────────────────────────────────────────────────────────
@app.get("/api/teams")
def list_teams():
    db = get_db()
    repo = TeamRepository(db)
    series_id = request.args.get("series_id", type=int)
    if series_id:
        teams = repo.get_by_series(series_id)
    else:
        teams = repo.get_all()
    data = [t.to_dict(include_series=True, include_drivers=True) for t in teams]
    db.close()
    return jsonify(data)


@app.get("/api/teams/<int:tid>")
def get_team(tid):
    db = get_db()
    t = TeamRepository(db).get_by_id(tid)
    if not t:
        db.close()
        abort(404)
    data = t.to_dict(include_series=True, include_drivers=True)
    db.close()
    return jsonify(data)


@app.post("/api/teams")
def create_team():
    data = request.get_json(force=True)
    db = get_db()
    t = TeamRepository(db).create(data)
    db.close()
    return jsonify(t.to_dict()), 201


@app.put("/api/teams/<int:tid>")
def update_team(tid):
    data = request.get_json(force=True)
    db = get_db()
    t = TeamRepository(db).update(tid, data)
    db.close()
    if not t:
        abort(404)
    return jsonify(t.to_dict())


@app.delete("/api/teams/<int:tid>")
def delete_team(tid):
    db = get_db()
    ok = TeamRepository(db).delete(tid)
    db.close()
    if not ok:
        abort(404)
    return jsonify({"deleted": tid})


@app.get("/api/teams/stats/avg-points")
def teams_avg_points():
    db = get_db()
    data = TeamRepository(db).avg_points_by_series()
    db.close()
    return jsonify(data)


# ─────────────────────────────────────────────────────────────────────────────
# DRIVERS
# ─────────────────────────────────────────────────────────────────────────────
@app.get("/api/drivers")
def list_drivers():
    db = get_db()
    repo = DriverRepository(db)
    team_id = request.args.get("team_id", type=int)
    series_id = request.args.get("series_id", type=int)
    if team_id:
        drivers = repo.get_by_team(team_id)
    elif series_id:
        drivers = repo.get_standings_by_series(series_id)
    else:
        drivers = repo.get_all()
    data = [d.to_dict(include_team=True) for d in drivers]
    db.close()
    return jsonify(data)


@app.get("/api/drivers/<int:did>")
def get_driver(did):
    db = get_db()
    d = DriverRepository(db).get_by_id(did)
    if not d:
        db.close()
        abort(404)
    data = d.to_dict(include_team=True)
    db.close()
    return jsonify(data)


@app.post("/api/drivers")
def create_driver():
    data = request.get_json(force=True)
    db = get_db()
    d = DriverRepository(db).create(data)
    db.close()
    return jsonify(d.to_dict()), 201


@app.put("/api/drivers/<int:did>")
def update_driver(did):
    data = request.get_json(force=True)
    db = get_db()
    d = DriverRepository(db).update(did, data)
    db.close()
    if not d:
        abort(404)
    return jsonify(d.to_dict())


@app.delete("/api/drivers/<int:did>")
def delete_driver(did):
    db = get_db()
    ok = DriverRepository(db).delete(did)
    db.close()
    if not ok:
        abort(404)
    return jsonify({"deleted": did})


@app.get("/api/drivers/stats/total-points")
def drivers_total_points():
    db = get_db()
    data = DriverRepository(db).total_points_by_series()
    db.close()
    return jsonify(data)


# ─────────────────────────────────────────────────────────────────────────────
@app.errorhandler(404)
def not_found(e):
    return jsonify({"error": str(e)}), 404


@app.errorhandler(400)
def bad_request(e):
    return jsonify({"error": str(e)}), 400


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000, debug=True)
