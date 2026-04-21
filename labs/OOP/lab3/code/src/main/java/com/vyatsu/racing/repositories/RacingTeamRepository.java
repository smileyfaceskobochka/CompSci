package com.vyatsu.racing.repositories;

import com.vyatsu.racing.models.RacingTeam;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RacingTeamRepository extends CrudRepository<RacingTeam, Long> {
}
