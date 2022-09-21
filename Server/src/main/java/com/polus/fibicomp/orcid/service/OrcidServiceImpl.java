package com.polus.fibicomp.orcid.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.awardprojectoutcome.dao.AwardProjectOutcomeDao;
import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.orcid.comparator.PersonOrcidWorkComparator;
import com.polus.fibicomp.orcid.dao.OrcidDao;
import com.polus.fibicomp.orcid.dto.OrcidVO;
import com.polus.fibicomp.orcid.dto.OrcidWorkLinkedAwardDto;
import com.polus.fibicomp.orcid.pojo.AwardPersonOrcidWork;
import com.polus.fibicomp.orcid.pojo.OrcidErrorLog;
import com.polus.fibicomp.orcid.pojo.OrcidWork;
import com.polus.fibicomp.orcid.pojo.PersonOrcidWork;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;

@Transactional
@Service(value = "orcidService")
public class OrcidServiceImpl implements OrcidService {

	protected static Logger logger = LogManager.getLogger(OrcidServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private OrcidDao orcidDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardProjectOutcomeDao awardProjectOutcomeDao;

	private static final String UPDATE_USER = "quickstart";

	@Override
	public String getPersonOrcidWorks(String personId) {
		OrcidVO vo = new OrcidVO();
		List<PersonOrcidWork> personOrcidWorks = new ArrayList<>();
		personOrcidWorks = orcidDao.fetchPersonOrcidWorksBypersonId(personId);
		if (personOrcidWorks != null && !personOrcidWorks.isEmpty()) {
			Collections.sort(personOrcidWorks, new PersonOrcidWorkComparator());
			for (PersonOrcidWork personOrcidWork : personOrcidWorks) {
				setCountryName(personOrcidWork);
				List<AwardPersonOrcidWork> awardPersonOrcidWorks = orcidDao.getAwardPersonOrcidWorksByPersonOrcidWorkId(personOrcidWork.getPersonOrcidWorkId());
				List<OrcidWorkLinkedAwardDto> linkedAwards = new ArrayList<OrcidWorkLinkedAwardDto>();
				if (awardPersonOrcidWorks != null && !awardPersonOrcidWorks.isEmpty()) {
					for (AwardPersonOrcidWork awardPersonOrcidWork : awardPersonOrcidWorks) {
						OrcidWorkLinkedAwardDto linkedAward = new OrcidWorkLinkedAwardDto();
						linkedAward.setAwardPersonOrcidWorkId(awardPersonOrcidWork.getAwardPersonOrcidWorkId());
						linkedAward.setPersonOrcidWorkId(awardPersonOrcidWork.getPersonOrcidWorkId());
						linkedAward.setPersonId(awardPersonOrcidWork.getPersonId());
						linkedAward.setAwardNumber(awardPersonOrcidWork.getAwardNumber());
						linkedAward.setAwardDetail(getAwardDetails(awardPersonOrcidWork.getAwardNumber()));
						linkedAwards.add(linkedAward);
					}
					personOrcidWork.setLinkedAwards(linkedAwards);
				}
			}
		}
		vo.setPersonOrcidWorks(personOrcidWorks);
		Person person = personDao.getPersonDetailById(personId);
		if (person != null) {
			vo.setPerson(person);
		}
		setAllOrcidWorkCategoriesAndTypes(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private ModuleDetails getAwardDetails(String awardNumber) {
		Award award = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
		if (award == null) {
			award = awardDao.fetchPendingAwardByAwardNumber(awardNumber);
		}
		Integer awardIdValue = award.getAwardId();
		ModuleDetails moduleDetail = new ModuleDetails();
		moduleDetail.setModuleId(award.getAwardId());
		moduleDetail.setModuleItemKey(award.getAwardNumber());
		moduleDetail.setTitle(award.getTitle());
		moduleDetail.setLeadUnitName(award.getLeadUnit().getUnitName());
		if (award.getSponsorAwardNumber() != null) {
			moduleDetail.setSponsorAwardNumber(award.getSponsorAwardNumber());
		}
		if (award.getAccountNumber() != null) {
			moduleDetail.setAccountNumber(award.getAccountNumber());
		}
		moduleDetail.setSponsorName(award.getSponsor().getSponsorName());
		AwardPerson awardPerson = awardProjectOutcomeDao.getAwardPiDetails(awardIdValue);
		if (awardPerson != null) {
			moduleDetail.setPiName(awardPerson.getFullName());
			if (awardPerson.getPersonId() != null) {
				moduleDetail.setPiPersonId(awardPerson.getPersonId());
			}
			if (awardPerson.getRolodexId() != null) {
				moduleDetail.setPiRolodexId(awardPerson.getRolodexId());
			}
		}
		return moduleDetail;
	}

	@Override
	public String getOrcidWorkById(Integer putCode) {
		OrcidWork orcidWork = orcidDao.getOrcidWorkById(putCode);
		return commonDao.convertObjectToJSON(orcidWork);
	}

	@Override
	public String linkPersonOrcidWorkToAward(OrcidVO vo) {
		if (vo.getPersonOrcidWorkIds() != null && !vo.getPersonOrcidWorkIds().isEmpty()) {
			List<AwardPersonOrcidWork> awardPersonOrcidWorks = new ArrayList<AwardPersonOrcidWork>();
			for (Integer personOrcidWorkId : vo.getPersonOrcidWorkIds()) {
				AwardPersonOrcidWork orcidWork = new AwardPersonOrcidWork();
				orcidWork.setPersonOrcidWorkId(personOrcidWorkId);
				orcidWork.setPersonId(vo.getPersonId());
				orcidWork.setAwardNumber(vo.getAwardNumber());
				orcidWork.setUpdateUser(vo.getUpdateUser());
				orcidWork.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				orcidWork = orcidDao.saveOrUpdateAwardPersonOrcidWork(orcidWork);
				orcidWork.setAwardDetail(getAwardDetails(orcidWork.getAwardNumber()));
				awardPersonOrcidWorks.add(orcidWork);
			}
			vo.setAwardPersonOrcidWorks(awardPersonOrcidWorks);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String unLinkPersonOrcidWorkFromAward(OrcidVO vo) {
		if (vo.getAwardPersonOrcidWorkId() != null) {
			AwardPersonOrcidWork orcidWork = orcidDao.getAwardPersonOrcidWorkById(vo.getAwardPersonOrcidWorkId());
			if (orcidWork != null) {
				orcidDao.deleteAwardPersonOrcidWork(orcidWork);
				vo.setMessage("Success");
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getLinkedOrcidWorksOfAward(OrcidVO vo) {
		String awardNumber = awardDao.getAwardNumberBasedOnAwardId(vo.getAwardId());
		List<AwardPersonOrcidWork> awardPersonOrcidWorks = orcidDao.getAwardPersonOrcidWorksByAwardNumber(awardNumber);
		if (awardPersonOrcidWorks != null && !awardPersonOrcidWorks.isEmpty()) {
			List<PersonOrcidWork> linkedOrcidWorks = new ArrayList<PersonOrcidWork>();
			for (AwardPersonOrcidWork awardPersonOrcidWork : awardPersonOrcidWorks) {
				PersonOrcidWork personOrcidWork = new PersonOrcidWork();
				personOrcidWork = awardPersonOrcidWork.getPersonOrcidWork();
				personOrcidWork.setPersonFullname(personDao.getPersonFullNameByPersonId(personOrcidWork.getPersonId()));
				setCountryName(personOrcidWork);
				linkedOrcidWorks.add(personOrcidWork);
			}
			vo.setPersonOrcidWorks(linkedOrcidWorks);
		}
		setAllOrcidWorkCategoriesAndTypes(vo);
		return commonDao.convertObjectToJSON(vo);
	}

	private void setCountryName(PersonOrcidWork personOrcidWork) {
		if (personOrcidWork.getOrcidWork().getCountryCode() != null) {
			String countryName = orcidDao.getCountryNameByCountryTwoCode(personOrcidWork.getOrcidWork().getCountryCode());
			personOrcidWork.getOrcidWork().setCountryName(countryName);
		}
	}

	@Override
	public void saveOrcidErrorLog(Integer statusCode, String message, String orcidId, String putCode) {
		OrcidErrorLog orcidErrorLog = new OrcidErrorLog();
		orcidErrorLog.setErrorType(statusCode.toString());
		orcidErrorLog.setErrorMessage(message);
		orcidErrorLog.setOrcidId(orcidId);
		orcidErrorLog.setUpdateUser(UPDATE_USER);
		orcidErrorLog.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		orcidDao.saveOrUpdateOrcidErrorLog(orcidErrorLog);
	}

	private void setAllOrcidWorkCategoriesAndTypes(OrcidVO vo) {
		vo.setOrcidWorkCategories(orcidDao.getAllOrcidWorkCateogories());
		vo.setOrcidWorkTypes(orcidDao.getAllOrcidWorkTypes());
	}

}
