INSERT INTO racing_series (id, name, official_name, headquarters, governing_body, founded_year, rounds)
VALUES
    (1, 'Formula 1', 'FIA Formula One World Championship', 'London, UK', 'FIA', 1950, 24),
    (2, 'Formula 2', 'FIA Formula 2 Championship', 'Paris, France', 'FIA', 2017, 14),
    (3, 'Formula E', 'ABB FIA Formula E World Championship', 'Geneva, Switzerland', 'FIA', 2014, 16)
ON CONFLICT (id) DO NOTHING;
