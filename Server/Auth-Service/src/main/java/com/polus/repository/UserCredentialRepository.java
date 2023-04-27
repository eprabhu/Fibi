package com.polus.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.polus.entity.Person;

import java.util.Optional;

public interface UserCredentialRepository  extends JpaRepository<Person,Integer> {
   
    public Optional<Person> findByPrincipalName(String principalName);
}
