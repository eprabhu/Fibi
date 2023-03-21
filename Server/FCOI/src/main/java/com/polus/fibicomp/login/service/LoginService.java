package com.polus.fibicomp.login.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.login.vo.LoginVO;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.vo.CommonVO;

/**
 * LoginService class for login functionality.
 *
 */
/**
 * @author Shaji P
 *
 */
@Transactional
@Service
public interface LoginService {

	/**
	 * This method is used to authenticate the user.
	 * @param loginMode - Login mode of the user.
	 * @param userName - User name of the user.
	 * @param password - Password of the user.
	 * @param request
	 * @param response
	 * @return Object of class PersonDTO containing details of the user.
	 * @throws Exception
	 */
	public PersonDTO loginCheck(String loginMode, String userName, String password, HttpServletRequest request, HttpServletResponse response) throws Exception;

	/**
	 * This method is used to find the role of the user.
	 * @param personId - ID of the user.
	 * @return A boolean value to indicate role of the user.
	 */
	//public boolean isUnitAdmin(String personId);

	/**
	 * This method is used to change the existing password.
	 * @param vo - object of CommonVO
	 * @return A String of details having updated message.
	 * @throws Exception
	 */
	public String changePassword(CommonVO vo) throws Exception;

	/**
	 * This method is used to fetch details of login/logout details of a person.
	 * @param loginVO - object of LoginVO
	 * @return loginVO.
	 */
	public String fetchPersonLoginDetails(LoginVO loginVO);

	/**
	 * This method is used to savePersonLoginDetails.
	 * @param loginStatus
	 * @param userName
	 * @param unitNumber 
	 */
	public void savePersonLoginDetails(String personID, String fullName, String loginStatus, String userName);

	/**
	 * This method is used to download User Activity Datas.
	 * @param vo - exportUserActivityDatas
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> exportUserActivityDatas(LoginVO vo);

	public String getUnitByPersonId(LoginVO vo, String personId);

	/**
	 * Get all system notifications based on current date
	 * @return
	 */
	public String fetchSystemAllNotification();

	/**
	 * This method is used to mark system notification as read
	 * @param vo
	 * @return
	 */
	public String markNotificationsAsRead(LoginVO vo);

}
