package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;

@Transactional
@Service
public class PersonRenderService implements EmailRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	public PersonDao personDao;

	@Override
	public Map<String, String> getPlaceHolderData(String personId) {
		Person person = personDao.getPersonDetailById(personId);
		return getPersonPlaceHolder(person);
	}

	private Map<String, String> getPersonPlaceHolder(Person person) {
		Map<String, String> placeHolder = new HashMap<>();
		String link = generateLinkToApplication(person.getPersonId());
		placeHolder.put("{FULL_NAME}", person.getFullName() != null ? person.getFullName() : "");
		placeHolder.put("{APPLICATION_URL}", link);
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.PERSON_MODULE_CODE);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.PERSON_SUBMODULE_CODE.toString();
	}

	public String generateLinkToApplication(String parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PERSON_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}
