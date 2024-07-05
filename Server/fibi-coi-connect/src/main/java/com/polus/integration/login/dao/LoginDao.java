package com.polus.integration.login.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.dto.PersonDTO;

@Transactional
@Service
public interface LoginDao {

	/**
	 * This method is used to read person data.
	 * 
	 * @param userName - Username of the user.
	 * @return A PersonDTO object.
	 */
	public PersonDTO readPersonData(String userName);

}