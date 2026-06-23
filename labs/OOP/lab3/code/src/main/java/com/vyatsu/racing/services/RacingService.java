package com.vyatsu.racing.services;

import com.vyatsu.racing.models.RacingDriver;
import com.vyatsu.racing.models.RacingSeries;
import com.vyatsu.racing.models.RacingTeam;
import com.vyatsu.racing.repositories.RacingDriverRepository;
import com.vyatsu.racing.repositories.RacingSeriesRepository;
import com.vyatsu.racing.repositories.RacingTeamRepository;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

@Service
public class RacingService {

    private final RacingSeriesRepository seriesRepository;
    private final RacingTeamRepository teamRepository;
    private final RacingDriverRepository driverRepository;

    public RacingService(RacingSeriesRepository seriesRepository, 
                         RacingTeamRepository teamRepository, 
                         RacingDriverRepository driverRepository) {
        this.seriesRepository = seriesRepository;
        this.teamRepository = teamRepository;
        this.driverRepository = driverRepository;
    }

    // Series
    public Iterable<RacingSeries> getAllSeries() {
        return seriesRepository.findAll();
    }

    public List<RacingSeries> getAllSeriesWithTeams() {
        List<RacingSeries> all = new ArrayList<>();
        seriesRepository.findAll().forEach(all::add);
        return all;
    }

    public RacingSeries getSeriesById(Long id) {
        return seriesRepository.findById(id).orElse(null);
    }

    public void saveSeries(RacingSeries series) {
        seriesRepository.save(series);
    }

    public void deleteSeries(Long id) {
        seriesRepository.deleteById(id);
    }

    // Teams
    public Iterable<RacingTeam> getAllTeams() {
        return teamRepository.findAll();
    }

    public RacingTeam getTeamById(Long id) {
        return teamRepository.findById(id).orElse(null);
    }

    public void saveTeam(RacingTeam team) {
        teamRepository.save(team);
    }

    public void deleteTeam(Long id) {
        teamRepository.deleteById(id);
    }

    // Drivers
    public Iterable<RacingDriver> getAllDrivers() {
        return driverRepository.findAll();
    }

    public List<RacingDriver> getDriverStandings() {
        List<RacingDriver> drivers = new ArrayList<>();
        driverRepository.findAll().forEach(drivers::add);
        drivers.sort((a, b) -> b.getPoints() - a.getPoints());
        return drivers;
    }

    public LinkedHashMap<RacingSeries, List<RacingDriver>> getStandingsBySeries() {
        LinkedHashMap<RacingSeries, List<RacingDriver>> map = new LinkedHashMap<>();
        List<RacingSeries> seriesList = new ArrayList<>();
        seriesRepository.findAll().forEach(seriesList::add);

        List<RacingDriver> allDrivers = new ArrayList<>();
        driverRepository.findAll().forEach(allDrivers::add);

        for (RacingSeries s : seriesList) {
            List<RacingDriver> seriesDrivers = new ArrayList<>();
            for (RacingDriver d : allDrivers) {
                if (d.getTeam() != null && d.getTeam().getSeries() != null && d.getTeam().getSeries().getId().equals(s.getId())) {
                    seriesDrivers.add(d);
                }
            }
            seriesDrivers.sort((a, b) -> b.getPoints() - a.getPoints());
            map.put(s, seriesDrivers);
        }
        return map;
    }

    public RacingDriver getDriverById(Long id) {
        return driverRepository.findById(id).orElse(null);
    }

    public void saveDriver(RacingDriver driver) {
        driverRepository.save(driver);
    }

    public void deleteDriver(Long id) {
        driverRepository.deleteById(id);
    }
}
