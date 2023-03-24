package com.polus.fibicomp.persontraining.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.persontraining.dao.PersonTrainingDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingAttachment;
import com.polus.fibicomp.persontraining.pojo.PersonTrainingComment;
import com.polus.fibicomp.person.vo.PersonVO;

@Service
@Transactional
public class PersonTrainingServiceImpl implements PersonTrainingService {
	
	@Autowired
	private PersonTrainingDao personTrainingDao;
	
	@Autowired
    private RolodexDao rolodexDao;
	
	@Autowired
	private PersonDao personDao;
	
	@Autowired
    private PrintService printService;
	
	@Autowired
	private CommonDao commonDao;
	
	private static final String SUCCESS = "success";
	
	protected static Logger logger = LogManager.getLogger(PersonTrainingServiceImpl.class.getName());

	@Override
	public String saveOrUpdatePersonTraining(PersonVO vo) {
		personTrainingDao.saveOrUpdatePersonTraining(vo.getPersonTraining());
		vo.getPersonTraining().setUpdateUserName(AuthenticatedUser.getLoginUserFullName());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getTrainingDashboard(PersonVO vo) {		
		return commonDao.convertObjectToJSON(personTrainingDao.getTrainingDashboard(vo));
	}

	@Override
	public String saveOrUpdateTrainingComments(PersonVO vo) {
		personTrainingDao.saveOrUpdateTrainingComments(vo.getPersonTrainingComment());
		vo.getPersonTrainingComment().setUpdateUserName(AuthenticatedUser.getLoginUserFullName());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteTrainingComments(Integer trainingCommentId) {
		personTrainingDao.deleteTrainingComments(trainingCommentId);
		return commonDao.convertObjectToJSON(SUCCESS);
	}

	@Override
	public String saveOrUpdateTrainingAttachment(MultipartFile[] files, String formDataJson) {
		PersonVO personVO = null;
		try {
			ObjectMapper mapper = new ObjectMapper();
			personVO = mapper.readValue(formDataJson, PersonVO.class);
			if (personVO.getPersonTrainingAttachment().getTrainingAttachmentId() == null && files != null && files.length > 0) {	
				PersonTrainingAttachment attachment = personVO.getPersonTrainingAttachment(); 
				attachment.setFileName(files[0].getOriginalFilename());
				attachment.setMimeType(files[0].getContentType());
				FileData fileData = new FileData();
				fileData.setAttachment(files[0].getBytes());
				fileData = commonDao.saveFileData(fileData);
				attachment.setFileDataId(fileData.getFileDataId());
				personTrainingDao.saveOrUpdateTrainingAttachment(attachment);	
				attachment.setUpdateUserName(AuthenticatedUser.getLoginUserFullName());
			} else {
				personTrainingDao.saveOrUpdateTrainingAttachment(personVO.getPersonTrainingAttachment());
			} 
		} catch (IOException e) {
			logger.error("error in saveOrUpdateTrainingAttachment : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(personVO);
	}

	@Override
	public String getPersonTrainingDetails(Integer trainingId) {
		Map<String, Object> personTraining = new HashMap<>();
		PersonTraining training = personTrainingDao.getPersonTraining(trainingId);
		training.setUpdateUserName(personDao.getUserFullNameByUserName(training.getUpdateUser()));
		personTraining.put("personTraining", setTrainingDetails(training));
		personTraining.put("personDetails", setPersonDetail(training.getPersonId(), training.getNonEmployee()));
		return commonDao.convertObjectToJSON(personTraining);
	}

	private PersonTraining setTrainingDetails(PersonTraining training) {
		getFullNameOfUpdateUserComments(training.getPersonTrainingComments());
		getFullNameOfUpdateUserAttachments(training.getPersonTrainingAttachments());
		return training;
	}
	
	private void getFullNameOfUpdateUserComments(List<PersonTrainingComment> comments) {
		Set<String> userName = comments.stream().map(PersonTrainingComment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			comments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}
	
	private void getFullNameOfUpdateUserAttachments(List<PersonTrainingAttachment> comments) {
		Set<String> userName = comments.stream().map(PersonTrainingAttachment::getUpdateUser).collect(Collectors.toSet());
		if (!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), Person::getFullName));
			comments.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	private Map<String, Object> setPersonDetail(String personId, Boolean nonEmployee) {
		Map<String, Object> personDetails = new HashMap<>();
		if (Boolean.FALSE.equals(nonEmployee)) {
			Person person = personDao.getPersonDetailById(personId);
			personDetails.put("name", person.getFullName());
			personDetails.put("unit", person.getUnit() != null ? person.getUnit().getUnitName() : null);
			personDetails.put("email", person.getEmailAddress());
			personDetails.put("primaryTitle", person.getPrimaryTitle());
			personDetails.put("address", person.getAddressLine1());
			personDetails.put("city", person.getCity());
			personDetails.put("country", person.getCountry());
			personDetails.put("officeLocation", person.getOfficeLocation());
			personDetails.put("officePhoneNumber", person.getOfficePhone());
			personDetails.put("postalCode", person.getPostalCode());
		} else {
			Rolodex rolodex = rolodexDao.getRolodexDetailById(Integer.parseInt(personId));
			personDetails.put("name", rolodex.getFullName());
			personDetails.put("unit", rolodex.getOrganizations() != null ? rolodex.getOrganizations().getOrganizationName() : null);
			personDetails.put("email", rolodex.getEmailAddress());
			personDetails.put("primaryTitle", rolodex.getTitle());
			personDetails.put("address", rolodex.getAddressLine1());
			personDetails.put("city", rolodex.getCity());
			personDetails.put("country", rolodex.getCountry());
			personDetails.put("officeLocation", rolodex.getOrganizations() != null ? rolodex.getOrganizations().getAddress() : null);
			personDetails.put("officePhoneNumber", rolodex.getPhoneNumber());
			personDetails.put("postalCode", rolodex.getPostalCode());
		}
		return personDetails;
	}

	@Override
	public String loadTrainingList(String searchString) {	
		return commonDao.convertObjectToJSON(personTrainingDao.loadTrainingList(searchString));
	}

	@Override
	public ResponseEntity<byte[]> downloadtrainingAttachment(int attachmentId) {
		PersonTrainingAttachment attachment = personTrainingDao.loadPersonTrainingAttachment(attachmentId);
		try {
			FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
			return printService.setAttachmentContent(attachment.getFileName(), fileData.getAttachment());
		} catch (Exception e) {
			logger.error("Exception in downloadtrainingAttachment {}", e.getMessage());
		}
		return null;
	}

	@Override
	public String deleteTrainingAttachment(Integer attachmentId) {
		personTrainingDao.deleteTrainingAttachment(attachmentId);
		return commonDao.convertObjectToJSON(SUCCESS);
	}

	@Override
	public String deletePersonTraining(Integer personTrainingId) {
		personTrainingDao.deletePersonTraining(personTrainingId);
		return commonDao.convertObjectToJSON(SUCCESS);
	}

}
