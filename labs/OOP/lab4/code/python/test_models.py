from models import RacingSeries, RacingTeam, RacingDriver


class TestRacingSeries:
    def test_to_dict(self):
        s = RacingSeries(id=1, name="Formula 1", official_name="FIA Formula One World Championship",
                         headquarters="London", governing_body="FIA", founded_year=1950, rounds=24)
        d = s.to_dict()
        assert d["id"] == 1
        assert d["name"] == "Formula 1"
        assert d["rounds"] == 24
        assert "teams" not in d

    def test_to_dict_with_teams(self):
        s = RacingSeries(id=1, name="F1", headquarters="L", rounds=20)
        t = RacingTeam(id=1, name="Ferrari", principal_name="Fred", base_location="Maranello",
                       points=100, series_id=1)
        s.teams = [t]
        d = s.to_dict(include_teams=True)
        assert "teams" in d
        assert len(d["teams"]) == 1


class TestRacingTeam:
    def test_to_dict(self):
        t = RacingTeam(id=1, name="McLaren", principal_name="Andrea Stella",
                       base_location="Woking", power_unit="Mercedes", points=150,
                       series_id=1, team_color="#ff8700")
        d = t.to_dict()
        assert d["name"] == "McLaren"
        assert d["points"] == 150
        assert d["team_color"] == "#ff8700"

    def test_to_dict_with_relations(self):
        s = RacingSeries(id=1, name="F1", headquarters="L", rounds=20)
        t = RacingTeam(id=1, name="Red Bull", principal_name="CH", base_location="MK",
                       points=200, series_id=1)
        t.series = s
        d = t.to_dict(include_series=True)
        assert "series" in d
        assert d["series"]["name"] == "F1"


class TestRacingDriver:
    def test_to_dict(self):
        d = RacingDriver(id=1, first_name="Oscar", last_name="Piastri",
                         racing_number=81, points=100, team_id=1)
        di = d.to_dict()
        assert di["full_name"] == "Oscar Piastri"
        assert di["racing_number"] == 81

    def test_to_dict_with_team(self):
        s = RacingSeries(id=1, name="F1", headquarters="L", rounds=20)
        t = RacingTeam(id=1, name="McLaren", principal_name="AS", base_location="W",
                       points=200, series_id=1)
        t.series = s
        d = RacingDriver(id=1, first_name="Lando", last_name="Norris",
                         racing_number=4, points=120, team_id=1)
        d.team = t
        di = d.to_dict(include_team=True)
        assert "team" in di
        assert di["team"]["name"] == "McLaren"
