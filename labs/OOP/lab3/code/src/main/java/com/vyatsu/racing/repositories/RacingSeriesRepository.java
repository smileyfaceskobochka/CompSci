package com.vyatsu.racing.repositories;

import com.vyatsu.racing.models.RacingSeries;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RacingSeriesRepository extends CrudRepository<RacingSeries, Long> {
}
