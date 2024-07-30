package com.polus.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.polus.entity.Person;

import jakarta.transaction.Transactional;

import java.util.Optional;

public interface UserCredentialRepository  extends JpaRepository<Person,String> {
   
    public Optional<Person> findByPrincipalName(String principalName);
   
    @Modifying
    @Transactional
    @Query("UPDATE Person p SET p.secret = :secret, p.updateTimestamp = CURRENT_TIMESTAMP WHERE p.personId = :userId")
    void updateSecret(String userId, String secret);
}
