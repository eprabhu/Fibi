package com.polus.fibicomp.general.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.general.dao.GeneralInformationDao;
import com.polus.fibicomp.general.pojo.HelpText;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.SponsorMaintenanceVO;

@Transactional
@Service(value = "generalInformationService")
public class GeneralInformationServiceImpl implements GeneralInformationService {

	protected static Logger logger = LogManager.getLogger(GeneralInformationServiceImpl.class.getName());

	@Autowired
	private GeneralInformationDao generalInformationDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private PersonDao personDao;

	@Override
	public SponsorMaintenanceVO fetchSponsorData(String sponsorCode) {
		SponsorMaintenanceVO sponsorMaintenanceVO = new SponsorMaintenanceVO();
		Sponsor sponsor = generalInformationDao.fetchSponsorData(sponsorCode);
		sponsorMaintenanceVO.setSponsorCode(sponsor.getSponsorCode());
		sponsorMaintenanceVO.setSponsorName(sponsor.getSponsorName());
		sponsorMaintenanceVO.setSponsorTypeCode(sponsor.getSponsorTypeCode());
		sponsorMaintenanceVO.setUnitNumber(sponsor.getUnitNumber());
		sponsorMaintenanceVO.setAcronym(sponsor.getAcronym());
		sponsorMaintenanceVO.setActive(sponsor.isActive());
		sponsorMaintenanceVO.setSponsorLocation(sponsor.getSponsorLocation());
		sponsorMaintenanceVO.setAddressLine1(sponsor.getAddressLine1());
		sponsorMaintenanceVO.setAddressLine2(sponsor.getAddressLine2());
		sponsorMaintenanceVO.setAddressLine3(sponsor.getAddressLine3());
		sponsorMaintenanceVO.setEmailAddress(sponsor.getEmailAddress());
		sponsorMaintenanceVO.setPhoneNumber(sponsor.getPhoneNumber());
		sponsorMaintenanceVO.setUpdateUser(personDao.getUserFullNameByUserName(sponsor.getUpdateUser()));
		sponsorMaintenanceVO.setUpdateTimestamp(sponsor.getUpdateTimestamp());
		sponsorMaintenanceVO.setUnit(sponsor.getUnit());
		sponsorMaintenanceVO.setCreateUser(sponsor.getCreateUser());
		sponsorMaintenanceVO.setRolodexId(sponsor.getRolodexId());
		if (sponsor.getRolodex() != null) {
			sponsorMaintenanceVO.setRolodexName(sponsor.getRolodex().getFullName());
		}
		sponsorMaintenanceVO.setSponsorTypes(generalInformationDao.fetchSponsorTypeLookup());
		sponsorMaintenanceVO.setAcType("U");
		sponsorMaintenanceVO.setCountryCode(sponsor.getCountryCode());
		sponsorMaintenanceVO.setCountry(sponsor.getCountry());
		sponsorMaintenanceVO.setSponsorGroup(sponsor.getSponsorGroup());
		return sponsorMaintenanceVO;
	}

	@Override
	public SponsorMaintenanceVO createNewSponsor() {
		SponsorMaintenanceVO sponsorMaintenanceVO = new SponsorMaintenanceVO();
		sponsorMaintenanceVO.setAcType("I");
		sponsorMaintenanceVO.setActive(true);
		sponsorMaintenanceVO.setSponsorTypes(generalInformationDao.fetchSponsorTypeLookup());
		return sponsorMaintenanceVO;
	}

	@Override
	public SponsorMaintenanceVO saveSponsor(SponsorMaintenanceVO sponsorMaintenanceVO) {
		Sponsor sponsor = setDataToSponsorPojo(sponsorMaintenanceVO);
		if ("I".equals(sponsorMaintenanceVO.getAcType())) {
			sponsor.setCreateUser(sponsorMaintenanceVO.getUpdateUser());
			String sponsorCode = generalInformationDao.nextSponsorCode();
			sponsor.setSponsorCode(sponsorCode);
			generalInformationDao.insertSponsorData(sponsor);
			sponsorMaintenanceVO.setSponsorCode(sponsorCode);
			sponsorMaintenanceVO.setAcType("U");
			sponsorMaintenanceVO.setResponseMessage("INSERTED");
		} else if ("U".equals(sponsorMaintenanceVO.getAcType())) {
			generalInformationDao.updateSponsorData(sponsor);
			sponsorMaintenanceVO.setResponseMessage("UPDATED");

		} else if ("D".equals(sponsorMaintenanceVO.getAcType())) {
			generalInformationDao.deleteSponsorData(sponsor);
			sponsorMaintenanceVO.setResponseMessage("DELETED");
		}
		sponsorMaintenanceVO.setUpdateTimestamp(sponsor.getUpdateTimestamp());
		sponsorMaintenanceVO.setUpdateUser(AuthenticatedUser.getLoginUserFullName());
		return sponsorMaintenanceVO;
	}

	private Sponsor setDataToSponsorPojo(SponsorMaintenanceVO sponsorMaintenanceVO) {
		Sponsor sponsor = new Sponsor();
		sponsor.setSponsorCode(sponsorMaintenanceVO.getSponsorCode());
		sponsor.setSponsorName(sponsorMaintenanceVO.getSponsorName());
		sponsor.setSponsorTypeCode(sponsorMaintenanceVO.getSponsorTypeCode());
		sponsor.setUnitNumber(sponsorMaintenanceVO.getUnitNumber());
		sponsor.setAcronym(sponsorMaintenanceVO.getAcronym());
		sponsor.setActive(sponsorMaintenanceVO.isActive());
		sponsor.setSponsorLocation(sponsorMaintenanceVO.getSponsorLocation());
		sponsor.setAddressLine1(sponsorMaintenanceVO.getAddressLine1());
		sponsor.setAddressLine2(sponsorMaintenanceVO.getAddressLine2());
		sponsor.setAddressLine3(sponsorMaintenanceVO.getAddressLine3());
		sponsor.setEmailAddress(sponsorMaintenanceVO.getEmailAddress());
		sponsor.setPhoneNumber(sponsorMaintenanceVO.getPhoneNumber());
		sponsor.setUpdateUser(sponsorMaintenanceVO.getUpdateUser());
		sponsor.setCreateUser(sponsorMaintenanceVO.getCreateUser() == null ? sponsorMaintenanceVO.getUpdateUser() : sponsorMaintenanceVO.getCreateUser());
		sponsor.setRolodexId(sponsorMaintenanceVO.getRolodexId());
		sponsor.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		sponsor.setUpdateUser(AuthenticatedUser.getLoginUserName());
		sponsor.setCountryCode(sponsorMaintenanceVO.getCountryCode());
		sponsor.setSponsorGroup(sponsorMaintenanceVO.getSponsorGroup());
		return sponsor;
	}

	@Override
	public String syncPersonRole() {
		return commonDao.convertObjectToJSON(generalInformationDao.syncPersonRole());
	}

	@Override
	public String getAllSponsors(CommonVO vo) {
		return commonDao.convertObjectToJSON(generalInformationDao.getAllSponsors(vo));
	}

	@Override
	public String fetchHelpText(CommonVO vo) {
	Map<String, HelpText> helpTextDatas = new HashMap<>();
	List<Integer> sectionCodes = vo.getSectionCodes();
	sectionCodes.forEach(sectionCode -> {
		List<Map<String, HelpText>> helpTextDetail = new ArrayList<>();
		HelpText helpText = generalInformationDao.gethelpText(vo.getModuleCode(), sectionCode);
		if (helpText != null) {
		String category = helpText.getCategory();
		List<HelpText> parentHelpTexts = generalInformationDao.gethelpTextBasedOnHelpTextId(helpText.getHelpTextId());
		if (parentHelpTexts != null && !parentHelpTexts.isEmpty()) {
			parentHelpTexts.stream().forEach(parentText -> {
				Map<String, HelpText> parentHelpText = new HashMap<>();
				List<Map<String, HelpText>> childHelpTextDetail = new ArrayList<>();
				String categories = parentText.getCategory();
				List<HelpText> childHelpTexts = generalInformationDao.gethelpTextBasedOnHelpTextId(parentText.getHelpTextId());
				if (childHelpTexts != null && !childHelpTexts.isEmpty()) {
				 childHelpTexts.stream().forEach(childHelpText -> {
					Map<String, HelpText> childValues = new HashMap<>();
						String categoryValue = childHelpText.getCategory();
						childValues.put(categoryValue, childHelpText);
						childHelpTextDetail.add(childValues);
					});	
				}
				parentText.setParentHelpTexts(childHelpTextDetail);
				parentHelpText.put(categories, parentText);
				helpTextDetail.add(parentHelpText);
		    });
		}
		helpText.setParentHelpTexts(helpTextDetail);
		helpTextDatas.put(category, helpText);
		}
	});
		return commonDao.convertObjectToJSON(helpTextDatas);
	}
	
	@Override
	public String getModulesConfiguration(String moduleCode) {
		Map<String, Object> response = new HashMap<String, Object>();
		if(moduleCode != null) {
			response.put("sectionConfig", generalInformationDao.getSectionConfiguration(moduleCode));
		} else {
			response.put("moduleConfig", generalInformationDao.getModuleConfiguration());
		}
		return commonDao.convertObjectToJSON(response);
	}

}
