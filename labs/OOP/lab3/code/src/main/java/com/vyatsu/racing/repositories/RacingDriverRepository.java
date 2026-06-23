package com.vyatsu.racing.repositories;

import com.vyatsu.racing.models.RacingDriver;
// import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
// import java.util.List;

@Repository
public interface RacingDriverRepository extends CrudRepository<RacingDriver, Long> {
    // List<RacingDriver> findAllByOrderByPointsDesc();

    // @EntityGraph(attributePaths = {"team", "team.series"})
    // List<RacingDriver> findByTeam_SeriesIdOrderByPointsDesc(Long seriesId);
}
