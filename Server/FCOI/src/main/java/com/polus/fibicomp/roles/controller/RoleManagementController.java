package com.polus.fibicomp.roles.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.roles.service.RoleManagementService;
import com.polus.fibicomp.roles.vo.CheckRightVO;
import com.polus.fibicomp.roles.vo.RoleManagementVO;
import com.polus.fibicomp.roles.vo.RoleVO;

import io.jsonwebtoken.Claims;

@RestController
public class RoleManagementController {

	protected static Logger logger = LogManager.getLogger(RoleManagementController.class.getName());

	@Autowired
	@Qualifier(value = "roleManagementService")
	private RoleManagementService roleManagementService;

	@Autowired
	@Qualifier(value = "authorizationService")
	private AuthorizationService authorizationService;

	@Autowired
	private CommonService commonService;

	private static final String ROLE_ID = "roleId : {}";

	@PostMapping(value = "/assignedRoleOfPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String assignedRoleOfPerson(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for assignedRoleOfPerson");
		logger.info("getPersonId : {}", vo.getPersonId());
		logger.info("getDepartmentId : {}", vo.getUnitNumber());
		return roleManagementService.getAssignedRoleOfPerson(vo.getPersonId(), vo.getUnitNumber());
	}

	@PostMapping(value = "/unassignedRoleOfPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String unassignedRoleOfPerson(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for unassignedRoleOfPerson");
		logger.info("getPersonId : {}", vo.getPersonId());
		logger.info("getDepartmentId : {}", vo.getUnitNumber());
		return roleManagementService.getUnAssignedRoleOfPerson(vo.getPersonId(), vo.getUnitNumber());
	}

	@PostMapping(value = "/roleInformation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getRoleInformation(@RequestBody RoleVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for roleInformation");
		logger.info(ROLE_ID, vo.getRoleId());
		return roleManagementService.getRoleInformation(vo.getRoleId());
	}

	@PostMapping(value = "/savePersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String savePersonRoles(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for savePersonRoles");
		return roleManagementService.savePersonRoles(vo);
	}

	@PostMapping(value = "/fetchAllRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllRoles(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllRoles");
		return roleManagementService.fetchAllRoles();
	}

	@PostMapping(value = "/getAssignedRole", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAssignedRole(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAssignedRole");
		return roleManagementService.getAssignedRole(vo);
	}

	@GetMapping(value = "/getAllSystemRights", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAllSystemRights(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAllSystemRights");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		String personId = claims.get(Constants.LOGIN_PERSON_ID).toString();
		logger.info("personId : {}", personId);
		return authorizationService.allSystemLevelPermission(personId);
	}

	@PostMapping(value = "/createRole", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createRole(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createRole");
		return roleManagementService.createRole(vo);
	}

	@PostMapping(value = "/saveOrUpdateRole", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveRole(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateRole");
		return roleManagementService.saveOrUpdateRole(vo);
	}

	@PostMapping(value = "/deleteRole", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteRole(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteRole");
		logger.info(ROLE_ID, vo.getRoleId());
		return roleManagementService.deleteRole(vo);
	}

	@PostMapping(value = "/checkPersonHasRight", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public Boolean isPersonHasRightInAnyDepartment(@RequestBody CheckRightVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("isPersonHasRightInAnyDepartment");
		return authorizationService.isPersonHasRightInaDocument(vo.getRightName(), vo.getPersonId(), vo.getModuleCode(),
				vo.getModuleItemKey());
	}

	@PostMapping(value = "/getAssignedAndUnassignedRights", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAssignedAndUnassignedRights(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for getAssignedAndUnassignedRights");	
		return roleManagementService.getAssignedAndUnassignedRights(vo);
	}

	@PostMapping(value = "/getRoleById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getRoleById(@RequestBody RoleManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info(ROLE_ID, vo.getRoleId());		
		return roleManagementService.getRoleById(vo);
	}

	@ResponseStatus(HttpStatus.BAD_REQUEST)
	@ExceptionHandler(MethodArgumentNotValidException.class)
	public Map<String, String> handleValidationExceptions(MethodArgumentNotValidException ex) {
		Map<String, String> errors = new HashMap<>();
		ex.getBindingResult().getAllErrors().forEach((error) -> {
			String fieldName = ((FieldError) error).getField();
			String errorMessage = error.getDefaultMessage();
			errors.put(fieldName, errorMessage);
		});
		return errors;
	}

}
