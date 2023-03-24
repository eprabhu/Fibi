package com.polus.fibicomp.rolodex.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.polus.fibicomp.person.pojo.Person;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.rolodex.vo.RolodexSearchResult;
import com.polus.fibicomp.rolodex.vo.RolodexVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "rolodexService")
public class RolodexServiceImpl implements RolodexService {

	protected static Logger logger = LogManager.getLogger(RolodexServiceImpl.class.getName());

	@Autowired
	private RolodexDao rolodexDao;

	@Autowired
	private CommonDao commonDao;

	private static final Boolean TRUE = true;
	private static final Boolean FALSE = false;

	@Override
	public List<RolodexSearchResult> findRolodex(String searchString) {
		return rolodexDao.findRolodex(searchString);
	}

	@Override
	public String getRolodexDetailById(RolodexVO vo) {
		Rolodex rolodex = rolodexDao.getRolodexDetailById(vo.getRolodexId());
		Organization organization = rolodex.getOrganizations();
		if (rolodex.getCountryCode() != null) {
			rolodex.setCountry(rolodexDao.fetchCountryByCountryCode(rolodex.getCountryCode()));
		} else if (organization != null && organization.getCountryCode() != null) {
			rolodex.setCountry(rolodexDao.fetchCountryByCountryCode(organization.getCountryCode()));
		}
		vo.setRolodex(rolodex);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateRolodex(RolodexVO vo) {
		Rolodex rolodex = vo.getRolodex();
		String acType = vo.getAcType();
		logger.info("acType : {} " , acType);
		Boolean emailExist = FALSE;
		if(rolodex.getRolodexId() != null) {
			 emailExist = rolodexDao.checkPersonEmailAddress(rolodex.getRolodexId(), rolodex.getEmailAddress()) == TRUE; 
		}
	 	if (acType != null &&( acType.equals("I") || acType.equals("U"))) {
			if (Boolean.TRUE.equals(rolodexDao.checkUniqueEmailAddress(rolodex.getEmailAddress()) == FALSE || Boolean.TRUE.equals(emailExist))) {
				if(Boolean.TRUE.equals(vo.getIsAddToAddressBook()) && commonDao.getParameterValueAsBoolean(Constants.ADDTO_ADDRESSBOOK_ACTIVE_BY_DEFAULT)) {
					rolodex.setActive(true);
				}
				if (acType.equals(Constants.acTypeInsert)) {
					rolodex.setCreateUserFullName(AuthenticatedUser.getLoginUserFullName());
				}
				vo.setRolodex(rolodexDao.saveOrUpdateRolodex(rolodex));
				vo.setMessage("Rolodex saved Successfully");
				vo.setAcType("U");
			} else {
				vo.setMessage("Email Address already exists");
				vo.setRolodex(null);
			}
		} else if (acType != null && acType.equals("D")) {
			rolodexDao.deleteRolodex(rolodex);
			vo.setMessage("Rolodex Deleted Successfully");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAllRolodexes(RolodexVO vo) {
		vo = rolodexDao.getAllRolodexes(vo);
		getFullNameOfCreateUser(vo.getRolodexes());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<Country> findCountryList(String searchString) {
		return commonDao.fetchCountryList(searchString);
	}

	private void getFullNameOfCreateUser(List<Rolodex> rolodexes) {
		Set<String> userName = rolodexes.stream().map(Rolodex::getCreateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			rolodexes.stream().filter(item -> item.getCreateUser() != null).filter(item ->
					collect.containsKey(item.getCreateUser().toUpperCase())).forEach(item ->
					item.setCreateUserFullName(collect.get(item.getCreateUser().toUpperCase())));
		}
	}

}
