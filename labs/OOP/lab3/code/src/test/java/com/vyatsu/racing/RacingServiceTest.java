package com.vyatsu.racing;

import com.vyatsu.racing.models.*;
import com.vyatsu.racing.repositories.RacingDriverRepository;
import com.vyatsu.racing.repositories.RacingSeriesRepository;
import com.vyatsu.racing.repositories.RacingTeamRepository;
import com.vyatsu.racing.services.RacingService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@RunWith(MockitoJUnitRunner.class)
public class RacingServiceTest {

    @Mock
    private RacingSeriesRepository seriesRepository;

    @Mock
    private RacingTeamRepository teamRepository;

    @Mock
    private RacingDriverRepository driverRepository;

    private RacingService service;

    @Before
    public void setUp() {
        service = new RacingService(seriesRepository, teamRepository, driverRepository);
    }

    @Test
    public void testGetAllSeries() {
        RacingSeries s = new RacingSeries();
        s.setId(1L);
        s.setName("Formula 1");
        when(seriesRepository.findAll()).thenReturn(Collections.singletonList(s));

        Iterable<RacingSeries> result = service.getAllSeries();
        assertTrue(result.iterator().hasNext());
        assertEquals("Formula 1", result.iterator().next().getName());
    }

    @Test
    public void testGetSeriesById() {
        RacingSeries s = new RacingSeries();
        s.setId(1L);
        s.setName("WEC");
        when(seriesRepository.findById(1L)).thenReturn(Optional.of(s));

        RacingSeries result = service.getSeriesById(1L);
        assertNotNull(result);
        assertEquals("WEC", result.getName());
    }

    @Test
    public void testGetSeriesByIdNotFound() {
        when(seriesRepository.findById(99L)).thenReturn(Optional.empty());
        assertNull(service.getSeriesById(99L));
    }

    @Test
    public void testSaveSeries() {
        RacingSeries s = new RacingSeries();
        s.setName("New Series");
        s.setHeadquarters("Test HQ");
        service.saveSeries(s);
        verify(seriesRepository, times(1)).save(s);
    }

    @Test
    public void testDeleteSeries() {
        service.deleteSeries(1L);
        verify(seriesRepository, times(1)).deleteById(1L);
    }

    @Test
    public void testSaveTeam() {
        RacingTeam t = new F1Team();
        t.setName("Test Team");
        t.setPrincipalName("TP");
        t.setBaseLocation("Loc");
        t.setPoints(100);
        service.saveTeam(t);
        verify(teamRepository, times(1)).save(t);
    }

    @Test
    public void testDeleteTeam() {
        service.deleteTeam(1L);
        verify(teamRepository, times(1)).deleteById(1L);
    }

    @Test
    public void testSaveDriver() {
        RacingDriver d = new RacingDriver();
        d.setFirstName("Test");
        d.setLastName("Driver");
        d.setRacingNumber(55);
        d.setPoints(10);
        service.saveDriver(d);
        verify(driverRepository, times(1)).save(d);
    }

    @Test
    public void testDeleteDriver() {
        service.deleteDriver(1L);
        verify(driverRepository, times(1)).deleteById(1L);
    }

    @Test
    public void testGetStandingsBySeries() {
        RacingSeries s1 = new RacingSeries();
        s1.setId(1L);
        s1.setName("F1");

        RacingDriver d1 = new RacingDriver();
        d1.setId(1L);
        d1.setFirstName("Max");
        d1.setLastName("Verstappen");
        d1.setPoints(250);

        F1Team team1 = new F1Team();
        team1.setId(10L);
        team1.setSeries(s1);
        d1.setTeam(team1);

        when(seriesRepository.findAll()).thenReturn(Collections.singletonList(s1));
        when(driverRepository.findAll()).thenReturn(Collections.singletonList(d1));

        LinkedHashMap<RacingSeries, List<RacingDriver>> standings = service.getStandingsBySeries();
        assertEquals(1, standings.size());
        assertTrue(standings.containsKey(s1));
        assertEquals(1, standings.get(s1).size());
        assertEquals("Max", standings.get(s1).get(0).getFirstName());
    }

    @Test
    public void testTeamV5Fields() {
        F1Team f1 = new F1Team();
        f1.setWins(10);
        f1.setPodiums(25);
        f1.setBudgetCap(145.0);
        f1.setConstructorPos(2);

        F2Team f2 = new F2Team();
        f2.setChassisModel("MCL39");
        f2.setGraduates(3);
        f2.setIsFeeder(false);

        FormulaETeam fe = new FormulaETeam();
        fe.setEnergyPartner("Shell");
        fe.setBatteryKwh(50.0);
        fe.setSustainScore(85);

        assertEquals(Integer.valueOf(10), f1.getWins());
        assertEquals(Integer.valueOf(25), f1.getPodiums());
        assertEquals(Double.valueOf(145.0), f1.getBudgetCap());
        assertEquals(Integer.valueOf(2), f1.getConstructorPos());
        assertEquals("MCL39", f2.getChassisModel());
        assertEquals(Integer.valueOf(3), f2.getGraduates());
        assertEquals(false, f2.getIsFeeder());
        assertEquals("Shell", fe.getEnergyPartner());
        assertEquals(Double.valueOf(50.0), fe.getBatteryKwh());
        assertEquals(Integer.valueOf(85), fe.getSustainScore());
    }
}
