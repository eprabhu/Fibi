package com.polus.repository;

import com.polus.entity.PersonLoginDetail;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;

@Transactional
public interface PersonLoginDetailRepository extends JpaRepository<PersonLoginDetail, Integer> {
}
