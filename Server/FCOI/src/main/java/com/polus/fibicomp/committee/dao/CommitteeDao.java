package com.polus.fibicomp.committee.dao;

import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.pojo.Unit;

@Service
public interface CommitteeDao {

	/**
	 * This method is used to fetch list of Lead Units.
	 * @return list of leadUnits.
	 */
	public List<Unit> fetchAllHomeUnits();

	/**
	 * This method is used to retrieve current time.
	 * @return currentTime.
	 */
	public Date getCurrentDate();

	/**
	 * This method is used to retrieve current Timestamp.
	 * @return current Timestamp.
	 */
	public Timestamp getCurrentTimestamp();

	/**
	 * This method is used to convert Object into JSON format.
	 * @param object - request object.
	 * @return response - JSON data.
	 */
	public String convertObjectToJSON(Object object);

	/**
	 * This method is used to load all employees.
	 * @return employeesList - list of employees.
	 */
	public List<Person> getAllEmployees();

	/**
	 * This method is used to load all non-employees.
	 * @return nonEmployeesList - list of non-employees.
	 */
	public List<Rolodex> getAllNonEmployees();

	/**
	 * This method is used to get person's(employee) detail.
	 * @param personId - Id of the person.
	 * @return person - Person Object.
	 */
	public Person getPersonDetailsById(String personId);

	/**
	 * This method is used to get person's(non-employee) detail. 
	 * @param rolodexId - Id of the person.
	 * @return rolodex - Rolodex Object.
	 */
	public Rolodex getRolodexById(Integer rolodexId);

}
