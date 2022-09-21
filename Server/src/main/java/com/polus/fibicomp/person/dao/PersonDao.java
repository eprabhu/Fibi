package com.polus.fibicomp.person.dao;

import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.person.pojo.DegreeType;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonDegree;
import com.polus.fibicomp.person.pojo.PersonRoleRT;
import com.polus.fibicomp.person.vo.PersonSearchResult;
import com.polus.fibicomp.person.vo.PersonVO;
import com.polus.fibicomp.roles.pojo.PersonRoles;

@Transactional
@Service
public interface PersonDao {

	public List<PersonRoleRT> fetchCreateProposalPersonRole(String personId, String rightName);

	public boolean fetchSuperUserPersonRole(String personId, String rightName);
	
	/**
	 * This method will check if the user has a permission in any department
	 * @param personId
	 * @param permissionName
	 * @return True : If has permission and false it doesn't have permission
	 * 
	 */
	boolean isPersonHasPermissionInAnyDepartment(String personId,String permissionName);

	/**
	 * This method will check if the user has a permission in a particular department
	 * @param personId
	 * @param permissionName
	 * @param unitNumber
	 * @return True : If has permission and false it doesn't have permission
	 */
	boolean isPersonHasPermission(String personId,String permissionName,String unitNumber);

	/**
	 * This method will return the person details
	 * @param personId
	 * @return corresponding Person object
	 */
	public Person getPersonDetailById(String personId);

	/**
	 * This method will return the list of personId and fullName based on searchString
	 * @param searchString
	 * @return list of PersonSearchResult object
	 */
	public List<PersonSearchResult> findPerson(String searchString);

	/**
	 * This method is used to save and update the person
	 * @param person
	 * @return updated person object
	 */
	public Person saveOrUpdatePerson(Person person);

	/**
	 * This method is used to return all the list of persons
	 * @param vo
	 * @return persons list 
	 */
	public PersonVO getAllPersons(PersonVO vo);

	/**
	 * This method is used to delete the person
	 * @param person
	 */
	public void deletePerson(Person person);

	public PersonVO getAllTrainings(PersonVO vo);

	/**
	 * This method is used to check username already exists or not
	 * @param principalName
	 * @return boolean value of parameter.
	 */
	public boolean checkUniquePrincipalName(String principalName);

	public String getUserFullNameByUserName(String userName);

	/**
	 * This method is used to return person id based on user name
	 * @param userName
	 * @return person id
	 */
	public String getPersonIdByUserName(String userName);

	/**
	 * This method is used to return person full name based on personId
	 * @param personId
	 * @return FullName
	 */
	public String getPersonFullNameByPersonId(String personId);

	/**
	 * This method is used to return person roles based on unit number and role id
	 * @param unitNumber
	 * @param roleId
	 * @return person roles
	 */
	public List<PersonRoles> getPersonRoleByUnitNumberAndRoleId(String unitNumber, Integer roleId);

	/**
	 * This method is used to fetch person role rights from right name and unit number 
	 * @param unitNumber
	 * @param rightName
	 * @return person role rights
	 */
	public List<PersonRoleRT> fetchPersonRoleRTByRightNameAndUnitNumber(String unitNumber, String rightName);

	/**
	 * This method is used to return person roles based on unit number and role id
	 * @param roleId
	 * @return person roles
	 */
	public List<PersonRoles> getPersonRolesByRoleId(Integer roleId);

	/**
	 * This method will return the person details
	 * @param principalName
	 * @return corresponding Person object
	 */
	public Person getPersonDetailByPrincipalName(String principalName);

	/**
	 * This method is used to return person roles based on unit number and role ids
	 * @param roleId
	 * @return person roles
	 */
	public Set<String> fetchPersonIdByParams(String unitNumber, List<Integer> roleIds);

	/**
	 * This method is used to fetch PersonId based on UnitId & RoleId.
	 * @param An object of unitNumber,roleId.
	 * @return An string of personId.
	 */
	public Set<String> fetchPersonIdByUnitIdAndRoleId(String unitNumber,Integer roleId);
	
	public String savePersonFromFeed(Person person);

	/**
	 * This method is used get manpower person details by person id
	 * @param personId.
	 * @return object of Manpower.
	 */
	public Manpower getManpowerDetailsbyPersonId(String personId);

	/**
	 * This method is used get inactive persons
	 * @return list of persons.
	 */
	public List<Person> getInactivePersons();

	/**
	 * This method is used get inactive manpower person details 
	 * @param personIds
	 * @param executionStartTime.
	 * @return list of object.
	 */
	public List<Object[]> getInactivePersonsFromManpower(Set<String> personIds, Timestamp executionStartTime);

	/**
	 * This method is used get password of a person
	 * @param personId
	 * @return 
	 */
	public String getPersonPassword(String personId);

	/**
	 * This method is used to check person is inactive or not
	 * @param personId
	 * @return true or false
	 */
	public boolean checkForPersonInactive(String personId);

	/**
	 * This method is used get unit number of a person
	 * @param personId
	 * @return 
	 */
	public String getPersonUnitNumberByPersonId(String personId);

	//public List<Person> getPersonsWithOrcidIds();

	/**
	 * This method is used to return person roles based on role id and right id
	 * @param roleId
	 * @param rightId
	 * @return person roles
	 */
	public List<PersonRoles> getPersonRolesByRoleAndRightId(Integer roleId,  Integer rightId);

	/**
	 * This method is used to get group admin's personId based on right and group.
	 * @param rightName
	 * @param adminGroupId
	 * @return
	 */
	public List<String> getGroupAdminPersonIdsByRightName(String rightName, Integer adminGroupId);

	/**
	 * This method is used to get all the degree details of a person.
	 * @param personId
	 * @return list of personDegree
	 */
	public List<PersonDegree> getAllPersonDegree(String personId);

	/**
	 * This method is used to save person degree details.
	 * @param personDegree object
	 */
	public PersonDegree savePersonDegree(PersonDegree personDegree);

	/**
	 * This method is used to delete degree of a person based on personDegreeId.
	 * @param personDegreeId
	 */
	public void deletePersonDegreeById(Integer personDegreeId);

	/**
	 * This method is used to get all degree type lookup.
	 * @param  
	 * @return list of degree type.
	 */
	public List<DegreeType> getDegreeType();

	/**
	 * This method is used to update Person principalName with usernameSymbol + principalName
	 *
	 * @param personId
	 */
    void updateOldPersonUsername(String personId);

	/**
	 * This method is used to fetch persons by username by excluding the old username with % character
	 *
	 * @param principalName Username
	 * @return List of Persons
	 */
	List<Person> fetchPersonsByLikeUserName(String principalName);
}
