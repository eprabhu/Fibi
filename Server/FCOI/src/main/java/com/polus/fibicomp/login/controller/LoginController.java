package com.polus.fibicomp.login.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.login.service.LoginService;
import com.polus.fibicomp.login.vo.LoginVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class LoginController {

	protected static Logger logger = LogManager.getLogger(LoginController.class.getName());

	@Autowired
	@Qualifier(value = "loginService")
	private LoginService loginService;

	@Autowired
	private CommonDao commonDao;

	/*@RequestMapping(value = "/login", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> loginUser(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.debug("Received request for login: ");
		HttpStatus status = HttpStatus.OK;
		String userName = vo.getUserName();
		String password = vo.getPassword();
		PersonDTO personDTO = loginService.loginCheck(login_mode, userName, password, request, response);
		if (!personDTO.isLogin()) {
			status = HttpStatus.BAD_REQUEST;
		}
		ObjectMapper mapper = new ObjectMapper();
		String responseData = mapper.writeValueAsString(personDTO);
		return new ResponseEntity<String>(responseData, status);
	}*/

	@PostMapping(value = "/signout", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String logout(@RequestBody LoginVO vo, ModelMap model, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for sign out");
		if (vo.getPersonId() != null && !vo.getPersonId().isEmpty() && vo.getUserName() != null	&& !vo.getUserName().isEmpty()) {
			loginService.savePersonLoginDetails(vo.getPersonId(), vo.getUserFullName(), Constants.LOGOUT_FLAG, vo.getUserName());
			return commonDao.convertObjectToJSON("SUCCESS");
		} else {
			return commonDao.convertObjectToJSON("FAILED");
		}
	}

	@PostMapping(value = "/changePassword")
	public String changePassword(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for changePassword");
		return loginService.changePassword(vo);
	}

	@PostMapping(value = "/getPersonLoginDetails")
	public String changePassword(@RequestBody LoginVO vo, HttpServletRequest request) {
		logger.info("Requesting for getPersonLoginDetails");
		return loginService.fetchPersonLoginDetails(vo);
	}

	@PostMapping(value = "/exportUserActivityDatas")
	public ResponseEntity<byte[]> exportUserActivityDatas(HttpServletRequest request, @RequestBody LoginVO vo) throws Exception {
		logger.info("Requesting for exportUserActivityDatas");
		return loginService.exportUserActivityDatas(vo);
	}

	@PostMapping(value = "/getHomeUnitDetails")
	public String getHomeUnitDetails(@RequestBody LoginVO vo, HttpServletRequest request) {
		logger.info("Requesting for getHomeUnitDetails");
		logger.info("Login personId: {}",AuthenticatedUser.getLoginPersonId());
		return loginService.getUnitByPersonId(vo,AuthenticatedUser.getLoginPersonId());
	}

	@GetMapping(value = "/fetchSystemAllNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchSystemAllNotification(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSystemAllNotification");
		return loginService.fetchSystemAllNotification();
	}

	@PostMapping(value = "/markNotificationsAsRead", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String markNotificationsAsRead(@RequestBody LoginVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for markNotificationsAsRead");
		return loginService.markNotificationsAsRead(vo);
	}

}
