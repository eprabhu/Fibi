package com.polus.service;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.polus.entity.Person;
import com.polus.repository.UserCredentialRepository;

@Service
public class PersonService {

	@Autowired
	private UserCredentialRepository repository;

	public Optional<Person> loadUserByUsername(String username) throws UsernameNotFoundException {
		return repository.findByPrincipalName(username);
	}

	void updateSecret(String userId, String secret) {
		repository.updateSecret(userId, secret);
	}
}
