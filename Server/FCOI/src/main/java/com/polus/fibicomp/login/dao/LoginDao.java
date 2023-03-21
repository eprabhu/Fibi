package com.polus.fibicomp.login.dao;

import java.sql.Timestamp;
import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.login.pojo.PersonLoginDetail;
import com.polus.fibicomp.login.pojo.PersonSystemNotificationMapping;
import com.polus.fibicomp.login.pojo.SystemNotification;
import com.polus.fibicomp.login.vo.LoginVO;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.pojo.UnitAdministrator;

@Transactional
@Service
public interface LoginDao {

	/**
	 * This method is used to authenticate the user.
	 * @param userName - Username of the user.
	 * @param password - Password of the user.
	 * @return PrincipalBo containing user details.
	 */
	public Person authenticate(String userName, String password);

	/**
	 * This method is used to read person data.
	 * @param userName - Username of the user.
	 * @return A PersonDTO object.
	 */
	public PersonDTO readPersonData(String userName);

	/**
	 * This method is used to find the role of the user.
	 * @param personId - ID of the user.
	 * @return A boolean value to specify the user role.
	 */
	public List<UnitAdministrator> isUnitAdmin(String personId);

	/**
	 * This method is used to update password.
	 * @param encryptedPWD - New password to update.
	 * @param personId - Logged in person Id. 
	 * @return An integer value to indicate the response.
	 */
	public Integer changePassword(String encryptedPWD, String personId);

	/**
	 * This method is used to get password of an user.
	 * @param personId - Logged in person Id.
	 * @return Principal Object.
	 * @throws Exception
	 */
	public Person getCurrentPassword(String personId) throws Exception;

	/**
	 * This method is to save Person login details
	 * @param personLoginDetail
	 */
	public void savePersonLoginDetail(PersonLoginDetail personLoginDetail);

	/**
	 * This method is used to fetch PersonLoginDetail object.
	 * @param personLoginDetailId - PersonLoginDetail id.
	 * @return personLoginDetail.
	 */
	public PersonLoginDetail getPersonLoginDetailById(Integer personLoginDetailId);
	
	/**
	 * To check is External user
	 * @param personId
	 * @return
	 */
	public Boolean isExternalUser(String personId);
	
	/**
	 * Fetch All person login details
	 * @param loginVO
	 * @return
	 */
	public LoginVO fetchAllPersonLoginDetails(LoginVO loginVO);

	/**
	 * For user Activity screen filter
	 * @param personId
	 * @param systemRights
	 * @return
	 */
	public List<Unit> getUnitsListByPersonIdAndRights(String personId, List<String> systemRights);

	/**
	 * Fetch person login details 
	 * @param loginVO
	 * @param personIds
	 * @return
	 */
	public LoginVO getPersonLoginDetails(LoginVO loginVO, List<String> personIds);

	/**
	 * Get recent login details of a person by Person Id
	 * @param personIds
	 * @return
	 */
	public PersonLoginDetail getRecentPersonLoginDetailByUserName(String userName);

	/**
	 * Get all system notification based on current date
	 * @param currentDate
	 * @return
	 */
	public List<SystemNotification> fetchSystemAllNotification(Timestamp currentDate);

	/**
	 * Get all person system notification based on system notification id and person id
	 * @param systemNotificationId
	 * @param personId
	 * @return
	 */
	public PersonSystemNotificationMapping getPersonSystemNotificationByNotificationId(Integer systemNotificationId, String personId);

	/**
	 * delete person system notification based on system notification id , person id and current date
	 * @param systemNotificationId
	 * @param personId
	 * @param currentDate
	 * @return
	 */
	public void deletePersonSystemNotificationMapping(Integer systemNotificationId, String personId);

	/**
	 * save or update person system notification
	 * @param personSystemNotificationMapping
	 * @return
	 */
	public void saveOrUpdatePersonSystemNotificationMapping(PersonSystemNotificationMapping personSystemNotificationMapping);

}
