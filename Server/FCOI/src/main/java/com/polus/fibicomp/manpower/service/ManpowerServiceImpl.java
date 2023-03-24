package com.polus.fibicomp.manpower.service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.spec.KeySpec;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.Period;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.expense.dao.AwardExpenseDao;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetail;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.customdataelement.dao.CustomDataElementDao;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.manpower.dao.ManpowerDao;
import com.polus.fibicomp.manpower.dto.AwardManpowerDto;
import com.polus.fibicomp.manpower.dto.ManpowerPersonSearchResult;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;
import com.polus.fibicomp.manpower.pojo.ManpowerJobProfileType;
import com.polus.fibicomp.manpower.pojo.ManpowerLogUser;
import com.polus.fibicomp.manpower.pojo.WorkdayManpowerInterface;
import com.polus.fibicomp.manpower.vo.ManpowerVO;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.manpowerintegration.pojo.AwardManpowerBaseSalaryHistory;
import com.polus.fibicomp.manpowerintegration.service.ManpowerIntegrationService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "manpowerService")
public class ManpowerServiceImpl  implements ManpowerService {

	protected static Logger logger = LogManager.getLogger(ManpowerServiceImpl.class.getName());

	@Autowired
	private ManpowerDao manpowerDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private RolodexDao rolodexDao;

	@Autowired
	private AwardExpenseDao awardExpenseDao;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;
	
	@Autowired
	private ExcelityService excelityService;

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Autowired
	private ManpowerIntegrationService manpowerIntegrationService;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Value("${manpower.excelity.saltValue}")
	private String saltValue;

	@Override
	public String fetchAllManpowerLookUpDatas(Integer awardId) {
		ManpowerVO manpowerVO = new ManpowerVO();
		try {
			manpowerVO.setManpowerBudgetReferenceTypes(manpowerDao.getManpowerBudgetReferenceType());
			manpowerVO.setManpowerCandidateTitleType(manpowerDao.getManpowerCandidateTitleType());
			manpowerVO.setManpowerCompensationType(manpowerDao.getManpowerCompensationType());
			manpowerVO.setManpowerInterfaceStatus(manpowerDao.getManpowerInterfaceStatus());
			manpowerVO.setManpowerInterfaceType(manpowerDao.getManpowerInterfaceType());
			manpowerVO.setManpowerPositionStatus(manpowerDao.getManpowerPositionStatus());
			manpowerVO.setManpowerResourceType(manpowerDao.getManpowerResourceType());
			manpowerVO.setManpowerTypes(manpowerDao.getManpowerTypes());
			manpowerVO.setIsManpowerCreated(manpowerDao.checkIfAwardManpowerIsExistBasedOnParams(awardId));
			manpowerVO.setManpowerCutOffDate(getManpowerConfigurationValueAsString(Constants.MANPOWER_CUT_OFF_DATE));
			manpowerVO.setManpowerInfoText(getManpowerConfigurationValueAsString(Constants.MANPOWER_INFO_TEXT));
			manpowerVO.setPositionIds(manpowerDao.getDistinctPositionIds(awardId));
			manpowerVO.setManpowerUpgradeTypes(manpowerDao.getManpowerUpgradeType());
		} catch(Exception e) {
			logger.error("error in fetchAllManpowerLookUpDatas {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(manpowerVO);
	}

	@Override
	public String getManpowerConfigurationValueAsString(String configurationKey) {
		ManpowerConfigurationData manpowerConfigurationData = manpowerDao.getManpowerConfigurationValue(configurationKey);
		return manpowerConfigurationData != null ? manpowerConfigurationData.getConfigurationValue() : null;
	}

	@Override
	public String createManpowerPlan(ManpowerVO vo) {
		AwardBudgetHeader awardBudgetHeader = manpowerDao.getAwardBudgetByVersionNumber(vo.getAwardId());
		if (awardBudgetHeader != null && awardBudgetHeader.getBudgetId() != null) {
			vo.setBudgetVersionNumber(awardBudgetHeader.getVersionNumber());
			vo.setAwardBudgetHeaderId(awardBudgetHeader.getBudgetId());
			setAwardManpowerBudgetDetails(vo,  Integer.parseInt(Constants.ZERO), Integer.parseInt(Constants.ZERO), awardBudgetHeader.getBudgetId(), awardBudgetHeader.getVersionNumber());
		}
		setAwardManpowerDetailsForOthers(vo, Integer.parseInt(Constants.ZERO),  Integer.parseInt(Constants.ZERO));
		vo.setManpowerCategory(prepareAwardManpowerDetails(vo));
		return commonDao.convertObjectToJSON(vo);
	}

	private void setAwardManpowerBudgetDetails(ManpowerVO vo, Integer actualHeadCount, Integer approvedHeadCount, Integer budgetId, Integer versionNumber) {
		List<String> wbsNumbers = new ArrayList<>();
		List<String> budgetDetailsIds = new ArrayList<>();
		List<AwardManpower> awardManpowerBudgetReferenceIds = manpowerDao.getBudgetReferenceNumbers(vo.getAwardId());
		awardManpowerBudgetReferenceIds.stream().forEach(awardManpowerBudgetReferenceId -> {
			if (awardManpowerBudgetReferenceId.getBudgetReferenceTypeCode().equals(Constants.MANPOWER_BUDGET_REFERENCE_WBS_TYPE)) {
				wbsNumbers.add(awardManpowerBudgetReferenceId.getBudgetReferenceNumber());
			} else {
				budgetDetailsIds.add(awardManpowerBudgetReferenceId.getBudgetReferenceNumber());
			}
		});
		List<AwardBudgetDetail> awardBudgetDetails = manpowerDao.getAwardBudgetDetailForManpower(budgetId, wbsNumbers, budgetDetailsIds);
		vo.setCreateUser(vo.getUpdateUser());
		awardBudgetDetails.stream().forEach(awardBudgetDetail -> {
			setManpowerBudgetCategoryDetails(vo, awardBudgetDetail, actualHeadCount, approvedHeadCount, versionNumber);
		});
	 }

    @Override
	public ManpowerVO setManpowerBudgetCategoryDetails(ManpowerVO vo, AwardBudgetDetail awardBudgetDetail, Integer actualHeadCount,Integer approvedHeadCount, Integer budgetVersionNumber) {
		if (awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_CODE_EOM)) {
			vo.setManpowerTypeCode(Constants.MANPOWER_TYPE_STAFF);
			setAwardManpowerDetails(awardBudgetDetail ,actualHeadCount, approvedHeadCount, budgetVersionNumber, vo);
			
		}
		if (awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_CODE_RSS)) {
			vo.setManpowerTypeCode(Constants.MANPOWER_TYPE_STUDENT);
			setAwardManpowerDetails(awardBudgetDetail , actualHeadCount, approvedHeadCount, budgetVersionNumber, vo);
		}
		return vo;
	}

	private String getbudgetReferenceNumber(AwardBudgetDetail awardBudgetDetail, ManpowerVO vo) {
		String budgetReferenceNumber;
		if (awardBudgetDetail.getInternalOrderCode() != null) {
			vo.setBudgetReferenceTypeCode(Constants.MANPOWER_BUDGET_REFERENCE_WBS_TYPE);
			budgetReferenceNumber = awardBudgetDetail.getInternalOrderCode();
		} else {
			vo.setBudgetReferenceTypeCode(Constants.MANPOWER_BUDGET_REFERENCE_AWARD_BUDGET_DETIAL_TYPE);
			budgetReferenceNumber = awardBudgetDetail.getBudgetDetailId().toString();
		}
		return budgetReferenceNumber;
	}

	private List<String> getManpowerTypeCodes() {
		List<String> manpowerTypeCodes = new ArrayList<>();
		manpowerTypeCodes.add(Constants.MANPOWER_TYPE_STAFF);
		manpowerTypeCodes.add(Constants.MANPOWER_TYPE_STUDENT);
		manpowerTypeCodes.add(Constants.MANPOWER_TYPE_OTHER);
		return manpowerTypeCodes;
	}

	@Override
    public AwardManpowerDto prepareAwardManpowerDetails(ManpowerVO vo) {
    	AwardManpowerDto awardManpowerDto = new AwardManpowerDto();
    	List<String> manpowerTypeCodes = getManpowerTypeCodes();
    	Map<String, List<AwardManpower>> awardManpowerMap = new TreeMap<>();
    	manpowerTypeCodes.stream().forEach(manpowerTypeCode -> {
    		String manpowerType = manpowerDao.getManpowerTypeBasedOnCode(manpowerTypeCode).getDescription();
			List<AwardManpower> awardManpowers = manpowerDao.getAwardManpowerDetails(vo.getAwardId(), manpowerTypeCode);
			if (awardManpowers != null && !awardManpowers.isEmpty()) {
			List<AwardManpower> awardManpowerDetails = new ArrayList<>();
			awardManpowers.stream().forEach(awardManpower -> {
				 AwardBudgetDetail awardBudgetDetail = prepareAwardBudgetDetail(awardManpower,vo);
				 if (awardBudgetDetail != null) {
					 awardManpower.setBudgetCategory(awardBudgetDetail.getBudgetCategory());
					 awardManpower.setCostElement(awardBudgetDetail.getCostElement());
					 awardManpower.setBudgetAmount(awardBudgetDetail.getLineItemCost());
					 awardManpower.setApprovedHeadCount(awardBudgetDetail.getQuantity() != null ? awardBudgetDetail.getQuantity().intValue() : 0);
				 }
				 awardManpower.setActualHeadCount(manpowerDao.countOfActiveManpowerResource(awardManpower.getAwardManpowerId()));
				 awardManpower.setManpowerType(manpowerDao.getManpowerTypeBasedOnCode(awardManpower.getManpowerTypeCode()));
				 setAwardManpowerResourceDetails(awardManpower);
				 awardManpowerDetails.add(awardManpower);
			 });
			awardManpowerMap.put(manpowerType, awardManpowerDetails);
    	}
    	awardManpowerDto.setAwardManpowerDetails(awardManpowerMap);
    	});
		return awardManpowerDto;
	}

	private AwardBudgetDetail prepareAwardBudgetDetail(AwardManpower awardManpower, ManpowerVO vo) {
		AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
		if (awardManpower.getBudgetReferenceTypeCode() != null) {
			if (awardManpower.getBudgetReferenceTypeCode().equals(Constants.MANPOWER_BUDGET_REFERENCE_WBS_TYPE) && awardManpower.getBudgetReferenceNumber() != null && vo.getAwardBudgetHeaderId() != null) {
				awardBudgetDetail = manpowerDao.getAwardBudgetDetailByParams(null, awardManpower.getBudgetReferenceNumber(), vo.getAwardBudgetHeaderId());
			} else if (awardManpower.getBudgetReferenceTypeCode().equals(Constants.MANPOWER_BUDGET_REFERENCE_AWARD_BUDGET_DETIAL_TYPE) && awardManpower.getBudgetReferenceNumber() != null) {
				awardBudgetDetail = manpowerDao.getAwardBudgetDetailByParams(Integer.parseInt(awardManpower.getBudgetReferenceNumber()), null, null);
			}
		}
		return awardBudgetDetail;
	}

	private void setAwardManpowerDetailsForOthers(ManpowerVO vo, Integer actualHeadCount, Integer approvedHeadCount) {
		vo.setManpowerTypeCode(Constants.MANPOWER_TYPE_OTHER);
		vo.setBudgetReferenceTypeCode(null);
		setAwardManpowerDetails(null, actualHeadCount, approvedHeadCount, vo.getBudgetVersionNumber(), vo);
	}

	@Override
	public ManpowerVO setAwardManpowerDetails(AwardBudgetDetail awardBudgetDetail, Integer actualHeadCount, Integer approvedHeadCount, Integer budgetVersionNumber, ManpowerVO vo) {
		AwardManpower awardManpower = new AwardManpower();
		String budgetReferenceNumber = null;
		if (awardBudgetDetail != null) {
			budgetReferenceNumber = getbudgetReferenceNumber(awardBudgetDetail, vo);
		}
		awardManpower.setActualHeadCount(actualHeadCount);
		if (awardBudgetDetail != null && awardBudgetDetail.getQuantity() != null) {
			approvedHeadCount = awardBudgetDetail.getQuantity().intValue();
		}
		awardManpower.setApprovedHeadCount(approvedHeadCount);
		awardManpower.setAwardId(vo.getAwardId());
		awardManpower.setAwardNumber(vo.getAwardNumber());
		if (budgetReferenceNumber != null) {
			awardManpower.setBudgetReferenceNumber(budgetReferenceNumber);
		}
		if (vo.getBudgetReferenceTypeCode() != null) {
			awardManpower.setBudgetReferenceTypeCode(vo.getBudgetReferenceTypeCode());
		}
		if (budgetVersionNumber != null) {
			awardManpower.setBudgetVersionNumber(budgetVersionNumber);
		}
		awardManpower.setCreateTimestamp(commonDao.getCurrentTimestamp());
		awardManpower.setManpowerTypeCode(vo.getManpowerTypeCode());
		awardManpower.setSequenceNumber(vo.getSequenceNumber());
		awardManpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		awardManpower.setModuleCode(Constants.AWARD_MODULE_CODE);
		awardManpower.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE);
		manpowerDao.saveOrUpdateAwardManpower(awardManpower);
		vo.setAwardManpowerDetail(awardManpower);
		vo.setAwardManpowerId(awardManpower.getAwardManpowerId());
		return vo;
	}

	private AwardManpower setAwardManpowerResourceDetails(AwardManpower awardManpower) {
		 List<AwardManpowerResource> resourceDetail = new ArrayList<>();
		 List<AwardManpowerResource> awardManpowerResources = manpowerDao.getAwardManpowerResources(awardManpower.getAwardManpowerId());
		 awardManpowerResources.stream().forEach(awardManpowerResource -> {
			resourceDetail.add(prepareAwardManpowerResourceData(awardManpowerResource, awardManpower));
			awardManpower.setAwardManpowerResource(resourceDetail);
		 });
		 if (!awardManpowerResources.isEmpty() && !awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_OTHER)) {
			 if (!awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_OTHER)) {
				 awardManpower.setSapCommittedAmount(manpowerDao.getSumOfCommittedAmount(awardManpower.getAwardManpowerId()));
			 }
			 awardManpower.setExpenseAmount(calculateSapExpenseAmount(awardManpower));
		 }
		 return awardManpower;
	}

	private AwardManpowerResource prepareAwardManpowerResourceData(AwardManpowerResource awardManpowerResource, AwardManpower awardManpower) {
		String unitNumber = null;
		String personStatus = null;
		Timestamp dateOfInactive = null;
		prepareAwardManpowerLookUpDetails(awardManpowerResource);
		if (awardManpowerResource.getPersonId() != null) {
			Person person = personDao.getPersonDetailById(awardManpowerResource.getPersonId());
			if ((person != null && person.getHomeUnit() != null) || (person != null && person.getStatus() != null)) {
				unitNumber = person.getHomeUnit();
				personStatus = person.getStatus();
				dateOfInactive = person.getDateOfInactive();
			}
			setPersonJobProfileTypeCode(awardManpowerResource, person);
		 } else if (awardManpowerResource.getRolodexId() != null) {
			  Rolodex rolodex = rolodexDao.getRolodexDetailById(awardManpowerResource.getRolodexId());
			  if (rolodex != null && rolodex.getOwnedByUnit() != null) {
			    	unitNumber = rolodex.getOwnedByUnit();
			    	personStatus = Boolean.toString(rolodex.isActive());
			  }
			  if (rolodex != null && rolodex.getOrganizations() != null) {
				  awardManpowerResource.setOrganization(rolodex.getOrganizations().getOrganizationName());
			  }
		 }
		 if (unitNumber != null) {
			awardManpowerResource.setDepartment(commonDao.getUnitName(unitNumber));
		 }
		 awardManpowerResource.setPersonStatus(personStatus);
		 awardManpowerResource.setDateOfInactive(dateOfInactive);
		 return awardManpowerResource;
	}

	private AwardManpowerResource setPersonJobProfileTypeCode(AwardManpowerResource awardManpowerResource, Person person) {
		if (person != null) {
			Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(person.getPersonId());
			if (manpower != null && manpower.getJobCode() != null) {
				ManpowerJobProfileType manpowerJobProfileType = manpowerDao.getManpowerJobProfileTypeById(manpower.getJobCode());
				if (manpowerJobProfileType != null) {
					awardManpowerResource.setCandidateType(manpowerJobProfileType.getDescription());
				}
			}
		}
		return awardManpowerResource;
	}

	private AwardManpowerResource prepareAwardManpowerLookUpDetails(AwardManpowerResource awardManpowerResource) {
		try {
			String personId = getPersonId(awardManpowerResource);
			if (personId != null) {
				Manpower manpower = manpowerDao.fetchManpowerPersonDetail(awardManpowerResource.getPositionId(), personId);
				awardManpowerResource.setManpower(manpower);
			}
			if (awardManpowerResource.getPositionStatusCode() != null) {
				awardManpowerResource.setManpowerPositionStatus(
						manpowerDao.getManpowerPositionStatusById(awardManpowerResource.getPositionStatusCode()));
			}
		} catch (Exception e) {
			logger.error("Error in prepareAwardManpowerLookUpDetails {}", e.getMessage());
		}
		return awardManpowerResource;
	}

	@Override
	public String fetchManpowerDetails(ManpowerVO vo) {
		AwardBudgetHeader awardBudgetHeader = manpowerDao.getAwardBudgetByVersionNumber(vo.getAwardId());
		if (awardBudgetHeader != null && awardBudgetHeader.getBudgetId() != null) {
			vo.setBudgetVersionNumber(awardBudgetHeader.getVersionNumber());
			vo.setAwardBudgetHeaderId(awardBudgetHeader.getBudgetId());
			setAwardManpowerBudgetDetails(vo,  Integer.parseInt(Constants.ZERO), Integer.parseInt(Constants.ZERO), awardBudgetHeader.getBudgetId(), awardBudgetHeader.getVersionNumber());
		}
		List<AwardManpower> manpowerOthers = manpowerDao.getAwardManpowerDetailsBasedTypeCodes(vo.getAwardId(), Constants.MANPOWER_TYPE_OTHER);
		if (manpowerOthers == null || manpowerOthers.isEmpty()) {
			setAwardManpowerDetailsForOthers(vo, Integer.parseInt(Constants.ZERO),  Integer.parseInt(Constants.ZERO));
		}		
		vo.setManpowerCategory(prepareAwardManpowerDetails(vo));
		vo.setAccountNumber(awardDao.getAccountNumberByAwardId(vo.getAwardId()));
		return commonDao.convertObjectToJSON(vo);
	}
 
	@Override
	public ManpowerVO saveOrUpdateManpowerResource(ManpowerVO vo) {
		try {
		AwardManpowerResource awardManpowerResource = vo.getAwardManpowerResource();
		awardManpowerResource.setModuleCode(Constants.AWARD_MODULE_CODE);
		awardManpowerResource.setSubModuleCode(Constants.MANPOWER_SUBMODULE_CODE);
		awardManpowerResource.setAwardNumber(vo.getAwardNumber());
		AwardBudgetHeader awardBudgetHeaderDetail = awardBudgetDao.getAwardBudgetHeaderByAwardId(vo.getAwardId());
		if (awardBudgetHeaderDetail != null) {
			vo.setAwardBudgetHeaderId(awardBudgetHeaderDetail.getBudgetId());
		}
		AwardManpower awardManpower = manpowerDao.fetchAwardManpowerDetailByAwardManpowerId(awardManpowerResource.getAwardManpowerId());
		if (awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF)) {
			awardManpowerResource.setIsResourceCreatedOrUpdated(Boolean.TRUE);
			saveAwardManpowerResourceDetails(awardManpower, awardManpowerResource, vo);
		} else if(awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STUDENT)) {
			saveAwardManpower(awardManpower, awardManpowerResource, vo);
		} else {
			saveManpowerResourceDetails(awardManpowerResource, vo, awardManpower);
		}
	} catch (Exception e) {
		logger.error("Error in saveOrUpdateManpowerResource {}", e.getMessage());
	}
	   return vo;
	}

	private void saveAwardManpowerResourceDetails(AwardManpower awardManpower, AwardManpowerResource awardManpowerResource, ManpowerVO vo) {
		if (Boolean.TRUE.equals(vo.getIsBaseSalaryFieldValuesChanged())) {
			setAwardManpowerValidations(vo, awardManpowerResource, awardManpower);
		} else {
			setInitialCommittedAmount(vo, awardManpower, awardManpowerResource.getCommittedCost(), awardManpowerResource.getPlannedSalary(), awardManpowerResource.getManpowerResourceId());
			if (vo.getIsReadyToHire() != null) {
				awardManpowerResource.setPositionStatusCode(Boolean.FALSE.equals(vo.getIsReadyToHire()) ? Constants.MANPOWER_POSITION_NOT_TRIGGERED : Constants.MANPOWER_READY_TO_TRIGGER_POSITION); 
			}
			setIsHiringOnExistingPositionStatus(vo.getIsHiringOnExistingPosition(), awardManpowerResource);
			if (vo.getIsBaseSalaryFieldValuesChanged() != null && Boolean.TRUE.equals(vo.getIsBaseSalaryFieldValuesChanged())) {
				setResourcePositionStatus(Constants.MANPOWER_ACTIVE, Constants.MANPOWER_PENDING_APPROVAL, awardManpowerResource);
			}
			awardManpowerResource.setMultiplierValueUsed(manpowerDao.getResourceMultiplierUsed(awardManpowerResource.getManpowerResourceId()));
			manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResource);
			setAwardManpowerDetails(vo, awardManpowerResource, awardManpower);
		}
	}

	private ManpowerVO setAwardManpowerValidations(ManpowerVO vo, AwardManpowerResource awardManpowerResource, AwardManpower awardManpower) {
		Timestamp startDate = awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate();
		Timestamp endDate = awardManpowerResource.getChargeEndDate() == null ? awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate();
		String result = manpowerDao.getAwardManpowerCostAllocation(awardManpowerResource.getPersonId(), awardManpowerResource.getCostAllocation(), startDate, endDate, vo.getAwardNumber());
		if (result.equals("Y")) {
			saveAwardManpower(awardManpower, awardManpowerResource, vo);
		} else {
			vo.setIsCostAllocationValidationExist(true);
			prepareManpowerCostAllocationValidation(awardManpowerResource, vo, Constants.YES);
		}
		return vo;
	}

	private ManpowerVO prepareManpowerCostAllocationValidation(AwardManpowerResource awardManpowerResource, ManpowerVO vo, String flag) {
		vo.setAwardNumber(vo.getAwardNumber());
		vo.setAwardManpowerResources(manpowerDao.getManpowerResourceBasedCostAllocation(
				awardManpowerResource.getPersonId(), awardManpowerResource.getChargeStartDate() == null ? awardManpowerResource.getPlanStartDate() : awardManpowerResource.getChargeStartDate(),
				awardManpowerResource.getChargeEndDate() == null ? awardManpowerResource.getPlanEndDate() : awardManpowerResource.getChargeEndDate(), vo.getAwardNumber(), flag));
		return vo;
	}

	private void saveAwardManpower(AwardManpower awardManpower, AwardManpowerResource awardManpowerResource, ManpowerVO vo) {
		String multiplier = findMultiplier(awardManpower.getAwardId());
		prepareAwardManpowerDetail(multiplier, awardManpower, awardManpowerResource, vo);
		setInitialCommittedAmount(vo, awardManpower, awardManpowerResource.getCommittedCost(), awardManpowerResource.getPlannedSalary(), awardManpowerResource.getManpowerResourceId());
		if (Boolean.FALSE.equals(vo.getIsCommittedCostValidationExist())
			&& !(awardManpowerResource.getUpgradeTypeCode() == null && awardManpowerResource.getPositionId() == null && vo.getAddManpowerCategoryType() != null && vo.getAddManpowerCategoryType().equals(Constants.EXISTING_MANPOWER))) {
			if (awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF)) {
				awardManpowerResource.setMultiplierValueUsed(new BigDecimal(multiplier));
				saveManpowerResourceDetails(awardManpowerResource, vo, awardManpower);
			} else {
				saveManpowerResourceDetails(awardManpowerResource, vo, awardManpower);
			}
		}
    }

	private void saveManpowerResourceDetails(AwardManpowerResource awardManpowerResource, ManpowerVO vo, AwardManpower awardManpower) {
		StringBuilder value = new StringBuilder();
		String awardNumber = null;
		if (awardManpowerResource.getManpowerResourceId() == null) {
			awardNumber = value.append(vo.getAwardNumber()).append("_").append(vo.getSequenceNumber().toString()).toString();
		}
		manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResource);
		if (awardNumber != null) {
			awardManpowerResource.setResourceUniqueId(awardNumber.concat("_").concat(awardManpowerResource.getManpowerResourceId().toString()));
			manpowerDao.updateManpowerResourceUniqueId(awardManpowerResource.getResourceUniqueId(), awardManpowerResource.getManpowerResourceId());
		}
		setAwardManpowerDetails(vo, awardManpowerResource, awardManpower);
	}

	private AwardManpowerResource prepareAwardManpowerDetail(String multiplier, AwardManpower awardManpower, AwardManpowerResource awardManpowerResource, ManpowerVO vo) {
		if (awardManpowerResource.getManpowerResourceId() == null && awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF)) {
			if (vo.getAddManpowerCategoryType().equals(Constants.EXISTING_MANPOWER)) {
				if (vo.getIsReadyToHire() != null) {
					awardManpowerResource.setPositionStatusCode(Boolean.FALSE.equals(vo.getIsReadyToHire()) ? Constants.MANPOWER_POSITION_NOT_TRIGGERED : Constants.MANPOWER_READY_TO_TRIGGER_POSITION);
				} else {
					awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_PENDING_APPROVAL);
				}
				if ((awardManpowerResource.getPositionId() != null && Boolean.TRUE.equals(manpowerDao.checkPositionIdExistInAwardManpower(vo.getAwardId(), awardManpowerResource.getPositionId(), null)))
						||(awardManpowerResource.getUpgradeTypeCode().equals(Constants.MANPOWER_UPGRADE_NEW_PROJECT_TYPE_CODE))) {
					awardManpowerResource.setPositionOwnedByAward(Constants.YES);
				}
			} else {
				awardManpowerResource.setPositionStatusCode(Boolean.FALSE.equals(vo.getIsReadyToHire()) ? Constants.MANPOWER_POSITION_NOT_TRIGGERED : Constants.MANPOWER_READY_TO_TRIGGER_POSITION);
				setIsHiringOnExistingPositionStatus(vo.getIsHiringOnExistingPosition(), awardManpowerResource);
				awardManpowerResource.setPositionOwnedByAward(Constants.YES);
			}
			awardManpowerResource.setPlannedSalary(calculatePlannedCost(awardManpowerResource, vo.getAwardId().toString(), Boolean.FALSE, multiplier));
			awardManpowerResource.setMultiplierValueUsed(new BigDecimal(multiplier));
		} else if (awardManpowerResource.getManpowerResourceId() != null && awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_STAFF)) {
			awardManpowerResource.setPlannedSalary(calculatePlannedCost(awardManpowerResource, vo.getAwardId().toString() ,Boolean.FALSE, multiplier));
			awardManpowerResource.setMultiplierValueUsed(new BigDecimal(multiplier));
			if (vo.getIsBaseSalaryFieldValuesChanged() != null && Boolean.TRUE.equals(vo.getIsBaseSalaryFieldValuesChanged())) {
				setResourcePositionStatus(Constants.MANPOWER_ACTIVE, Constants.MANPOWER_PENDING_APPROVAL, awardManpowerResource);
			}
			if ((vo.getAddManpowerCategoryType().equals(Constants.NEW_MANPOWER) && vo.getIsReadyToHire() != null) || (vo.getIsReadyToHire() != null && awardManpowerResource.getPositionId() == null)) {
				awardManpowerResource.setPositionStatusCode(Boolean.FALSE.equals(vo.getIsReadyToHire()) ? Constants.MANPOWER_POSITION_NOT_TRIGGERED : Constants.MANPOWER_READY_TO_TRIGGER_POSITION);
			}
			setIsHiringOnExistingPositionStatus(vo.getIsHiringOnExistingPosition(), awardManpowerResource);
		}
		return awardManpowerResource;
	}

	private AwardManpowerResource setIsHiringOnExistingPositionStatus(Boolean isHiringOnExistingPosition, AwardManpowerResource awardManpowerResource) {
		if (isHiringOnExistingPosition != null && Boolean.TRUE.equals(isHiringOnExistingPosition)) {
			awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION); 
		}
		return awardManpowerResource;
	}

	private AwardManpowerResource setResourcePositionStatus(String activePositionStatus, String pendingPositionStatus, AwardManpowerResource awardManpowerResource) {
		if (awardManpowerResource.getPositionStatusCode().equals(activePositionStatus)) {
			awardManpowerResource.setPositionStatusCode(pendingPositionStatus);
			awardManpowerResource.setCommittedCost(null);
		}
		return awardManpowerResource;
	}

	private void setAwardManpowerDetails(ManpowerVO vo, AwardManpowerResource awardManpowerResource, AwardManpower awardManpower) {
		vo.setUpdateUser(awardManpowerResource.getUpdateUser());
		List<String> positionIds = manpowerDao.getDistinctPositionIds(vo.getAwardId());
		if (positionIds != null && !positionIds.isEmpty()) {
			vo.setPositionIds(positionIds);
		}
		vo.setAwardManpowerDetail(awardManpower);
		vo.setAwardManpowerResource(awardManpowerResource);
		vo.setManpowerResourceId(awardManpowerResource.getManpowerResourceId());
	}

	@Override
	public  ManpowerVO setInitialCommittedAmount(ManpowerVO vo, AwardManpower awardManpower, BigDecimal resourceCommittedCost, BigDecimal manpowerResourceAmount, Integer awardManpowerResourceId) {
		BigDecimal committedCost = BigDecimal.ZERO;
			List<Integer> resourceIds = manpowerDao.getAwardManpowerResourcesByParam(awardManpower.getAwardManpowerId());
			if (resourceIds != null && !resourceIds.isEmpty()) {
				for (Integer resourceId : resourceIds) {
					BigDecimal actualCommittedCost = manpowerDao.getSumOfResourcesCommittedAmount(awardManpower.getAwardManpowerId(), resourceId);
					if (actualCommittedCost != null) {
						committedCost = committedCost.add(actualCommittedCost);
					} else {
						BigDecimal initialCommittedCost = manpowerDao.getSumOfResourcesPlannedSalary(awardManpower.getAwardManpowerId(), resourceId);
						committedCost = committedCost.add(initialCommittedCost);
					}
				}
			}
			if (awardManpowerResourceId != null) {
				BigDecimal actualCommittedCost = manpowerDao.getSumOfResourcesCommittedAmount(awardManpower.getAwardManpowerId(), awardManpowerResourceId);
				if (actualCommittedCost != null) {
					committedCost = committedCost.subtract(actualCommittedCost);
				} else {
					BigDecimal initialCommittedCost = manpowerDao.getSumOfResourcesPlannedSalary(awardManpower.getAwardManpowerId(), awardManpowerResourceId);
					committedCost = committedCost.subtract(initialCommittedCost);
				}
			}
			if (resourceCommittedCost != null) {
				committedCost = committedCost.add(resourceCommittedCost);
			} else if (manpowerResourceAmount != null) {
				committedCost = committedCost.add(manpowerResourceAmount);
			}
			if (committedCost.compareTo(vo.getBudgetAmount()) > 0) {
				vo.setIsSalaryValidationExist(true);
				vo.setValidatedPlannedAmount(committedCost);
			}
		return vo;
	}

	@Override
	public String updateManpowerDetails(ManpowerVO vo) {
		AwardManpower awardManpower = vo.getAwardManpowerDetail();
		AwardManpowerResource awardManpowerResource = vo.getAwardManpowerResource();
		if (awardManpower != null && awardManpowerResource != null) {
			Integer actualHeadCount = manpowerDao.countOfActiveManpowerResource(awardManpower.getAwardManpowerId());
			awardManpower.setActualHeadCount(actualHeadCount);
			awardManpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardManpower.setUpdateUser(vo.getUpdateUser());
			manpowerDao.saveOrUpdateAwardManpower(awardManpower);
			awardManpower.setManpowerType(manpowerDao.getManpowerTypeBasedOnCode(awardManpower.getManpowerTypeCode()));
			vo.setAwardManpowerResource(prepareAwardManpowerResourceData(awardManpowerResource, awardManpower));
			if (!awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_OTHER)) {
				awardManpower.setSapCommittedAmount(manpowerDao.getSumOfCommittedAmount(awardManpower.getAwardManpowerId()));
			}
			awardManpower.setExpenseAmount(calculateSapExpenseAmount(awardManpower));
			vo.setAwardManpowerDetail(awardManpower);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private BigDecimal calculateSapExpenseAmount(AwardManpower awardManpower) {
		BigDecimal expenseAmount = BigDecimal.ZERO;
    	Award award = awardDao.getAwardDetailsById(awardManpower.getAwardId());
		if (award != null) {
			if (awardManpower.getBudgetReferenceTypeCode() != null && awardManpower.getBudgetReferenceTypeCode().equals(Constants.MANPOWER_BUDGET_REFERENCE_WBS_TYPE)) {
				AwardExpenseDetail awardExpenseDetail = awardExpenseDao.fetchAwardExpenseDetailByParams(award.getAwardNumber(), award.getAccountNumber(), awardManpower.getBudgetReferenceNumber());
				if (awardExpenseDetail != null) {
					expenseAmount = awardExpenseDetail.getTotalExpenseAmount() != null ? awardExpenseDetail.getTotalExpenseAmount() : BigDecimal.ZERO;
				}
			}
		}
		return expenseAmount;
	}

	private int getManpowerNumberOfDayInMonth(Date endDate) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(endDate);
		return calendar.getActualMaximum(Calendar.DAY_OF_MONTH);
	}

	private Period calculateManpowerDuration(Timestamp startTime, Timestamp endTime) {
		Date startDate = commonDao.adjustTimezone(new Date(startTime.getTime()));
		Date endDate = commonDao.adjustTimezone(new Date(endTime.getTime()));
		LocalDate localStartDate = new Timestamp(startDate.getTime()).toLocalDateTime().toLocalDate();
		LocalDate localEndDate = new Timestamp(endDate.getTime()).toLocalDateTime().toLocalDate().plusDays(1);
		return Period.between(localStartDate, localEndDate);
	}

	@Override
	public BigDecimal calculatePlannedCost(AwardManpowerResource awardManpowerResource, String awardId, Boolean isActualCommittedAmount, String multiplier) {
		int noOfDayInMonth = 0;
		int noOfDays = 0;
		int noOfMonths = 0;
		if(awardManpowerResource.getChargeEndDate() != null && awardManpowerResource.getChargeStartDate() != null) {
			noOfDayInMonth = getManpowerNumberOfDayInMonth(awardManpowerResource.getChargeEndDate());
			Period difference = calculateManpowerDuration(awardManpowerResource.getChargeStartDate(), awardManpowerResource.getChargeEndDate());
			noOfDays = difference.getDays();
			noOfMonths = difference.getMonths();
			noOfMonths = getNumberOfMonthsFromYear(difference, noOfMonths);
		} else if(awardManpowerResource.getPlanStartDate() != null && awardManpowerResource.getPlanEndDate() != null) {
			noOfDayInMonth = getManpowerNumberOfDayInMonth(awardManpowerResource.getPlanEndDate());
			Period difference = calculateManpowerDuration(awardManpowerResource.getPlanStartDate(), awardManpowerResource.getPlanEndDate());
			noOfDays = difference.getDays();
			noOfMonths = difference.getMonths();
			noOfMonths = getNumberOfMonthsFromYear(difference, noOfMonths);
		}
		return calculateManpowerCost(awardManpowerResource, awardId, noOfDayInMonth, noOfDays, noOfMonths, isActualCommittedAmount, multiplier);
	}

	private int getNumberOfMonthsFromYear(Period difference, Integer noOfMonths) {
		int monthsInYear = 12;
		if (((difference.getYears() != 0 && noOfMonths == 0) || (difference.getYears() != 0 && noOfMonths != 0)) && !(difference.getYears() == 0 && noOfMonths == 0)) {
			noOfMonths = noOfMonths + monthsInYear * difference.getYears();
		}
		return noOfMonths;
	}

	private BigDecimal calculateManpowerCost(AwardManpowerResource awardManpowerResource,String awardId, Integer noOfDayInMonth, Integer noOfDays, Integer noOfMonths, Boolean isActualCommittedAmount, String multiplier) {
		BigDecimal calculateManpowerAmount = new BigDecimal(0);
		if (isActualCommittedAmount) {
			calculateManpowerAmount = calculateActualCommittedAmountWithCustomData(noOfMonths, noOfDays, awardManpowerResource, noOfDayInMonth, multiplier);
		}else {
			calculateManpowerAmount = calculateCommittedAmountWithCustomData(noOfMonths, noOfDays, awardManpowerResource.getPlannedBaseSalary(), awardManpowerResource.getCostAllocation(), noOfDayInMonth, multiplier);
		}
		return calculateManpowerAmount;
	}

	private BigDecimal calculateCommittedAmountWithCustomData(int noOfMonths, int noOfDays, BigDecimal plannedSalary, BigDecimal costAllocation, int noOfDayInMonth, String customDataValue) {
		BigDecimal resourcePlannedCommittedAmount = null;
		BigDecimal hundred = new BigDecimal(100);
		if (noOfMonths != 0 && noOfDays != 0) {
			BigDecimal	monthlySalary = new BigDecimal(noOfMonths).multiply(plannedSalary).multiply(new BigDecimal(customDataValue)).multiply(costAllocation).divide(hundred, 2, RoundingMode.HALF_UP);
			BigDecimal 	plannedMonthlySalary = new BigDecimal(noOfDays).multiply(plannedSalary).multiply(new BigDecimal(customDataValue)).multiply(costAllocation).divide(hundred, 2, RoundingMode.HALF_UP).divide(new BigDecimal(noOfDayInMonth), 2, RoundingMode.HALF_UP);
			resourcePlannedCommittedAmount = monthlySalary.add(plannedMonthlySalary);
	     } else if(noOfDays == 0 && noOfMonths != 0) {
	    	 resourcePlannedCommittedAmount = new BigDecimal(noOfMonths).multiply(plannedSalary).multiply(new BigDecimal(customDataValue)).multiply(costAllocation).divide(hundred, 2, RoundingMode.HALF_UP);
	     } else if(noOfDays != 0) {
	    	 resourcePlannedCommittedAmount = plannedSalary.divide(new BigDecimal(noOfDayInMonth), 2, RoundingMode.HALF_UP).multiply(new BigDecimal(noOfDays)).multiply(new BigDecimal(customDataValue)).multiply(costAllocation).divide(hundred, 2, RoundingMode.HALF_UP);
	     }
		return resourcePlannedCommittedAmount;
	}
	
	private BigDecimal calculateActualCommittedAmountWithCustomData(int noOfMonths, int noOfDays, AwardManpowerResource resource, int noOfDayInMonth, String customDataValue) {
		BigDecimal resourceActualCommittedAmount = null;
		Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(resource.getPersonId());
		BigDecimal actualSalary = null;
		if (manpower != null && manpower.getBaseSalary() != null) {
			try {
				actualSalary =  new BigDecimal(excelityService.decryptAESData(manpower.getBaseSalary()));
			} catch (Exception e) {
				logger.error("error in decryptAESData for BaseSalary {}", e.getMessage());
			}
		}
		BigDecimal hundred = new BigDecimal(100);
		if (actualSalary != null) {
			if (noOfMonths != 0 && noOfDays != 0) {
				BigDecimal	monthlySalary = new BigDecimal(noOfMonths).multiply(actualSalary).multiply(new BigDecimal(customDataValue)).multiply(resource.getCostAllocation()).divide(hundred, 2, RoundingMode.HALF_UP);
				BigDecimal 	plannedMonthlySalary = new BigDecimal(noOfDays).multiply(actualSalary).multiply(new BigDecimal(customDataValue)).multiply(resource.getCostAllocation()).divide(hundred, 2, RoundingMode.HALF_UP).divide(new BigDecimal(noOfDayInMonth), 2, RoundingMode.HALF_UP);
				resourceActualCommittedAmount = monthlySalary.add(plannedMonthlySalary);
		     } else if(noOfDays == 0 && noOfMonths != 0) {
		    	 resourceActualCommittedAmount = new BigDecimal(noOfMonths).multiply(actualSalary).multiply(new BigDecimal(customDataValue)).multiply(resource.getCostAllocation()).divide(hundred, 2, RoundingMode.HALF_UP);
		     } else if(noOfDays != 0) {
		    	 resourceActualCommittedAmount = actualSalary.divide(new BigDecimal(noOfDayInMonth), 2, RoundingMode.HALF_UP).multiply(new BigDecimal(noOfDays)).multiply(new BigDecimal(customDataValue)).multiply(resource.getCostAllocation()).divide(hundred, 2, RoundingMode.HALF_UP);
		     }
		}
		return resourceActualCommittedAmount;
	}
	
	
	

	private String getPersonId(AwardManpowerResource awardManpowerResource) {
		String personId = null;
		if (awardManpowerResource.getPersonId() != null) {
			personId = awardManpowerResource.getPersonId();
		} else if(awardManpowerResource.getRolodexId() != null) {
			personId = awardManpowerResource.getRolodexId().toString();
		}
		return personId;
	}

	@Override
	public String saveOrUpdateAwardManpower(ManpowerVO vo) {
		AwardManpower awardManPower = manpowerDao.fetchAwardManpowerDetailByAwardManpowerId(vo.getAwardManpowerId());
		awardManPower.setUpdateUser(vo.getUpdateUser());
		awardManPower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		return commonDao.convertObjectToJSON(manpowerDao.saveOrUpdateAwardManpower(awardManPower));
	}

	@Override
	public String deleteManpowerResource(ManpowerVO vo) {
		AwardManpower awardManpower = manpowerDao.fetchAwardManpowerDetailByAwardManpowerId(vo.getAwardManpowerId());
		Integer actualHeadCount = manpowerDao.countOfActiveManpowerResource(awardManpower.getAwardManpowerId());
		AwardManpowerResource awardManpowerResource = manpowerDao.getAwardManpowerResourceById(vo.getManpowerResourceId());
		if (awardManpowerResource.getPositionId() != null && actualHeadCount > 0) {
			awardManpower.setActualHeadCount(actualHeadCount - 1);
			awardManpower.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardManpower.setUpdateUser(vo.getUpdateUser());
			manpowerDao.saveOrUpdateAwardManpower(awardManpower);
		}
		vo.setMessage(manpowerDao.deleteManpowerResource(vo.getManpowerResourceId()));
		if (!awardManpower.getManpowerTypeCode().equals(Constants.MANPOWER_TYPE_OTHER)) {
			awardManpower.setSapCommittedAmount(manpowerDao.getSumOfCommittedAmount(awardManpower.getAwardManpowerId()));
		}
		awardManpower.setExpenseAmount(calculateSapExpenseAmount(awardManpower));
		vo.setAwardManpowerDetail(awardManpower);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateManpowerTriggerDetail(ManpowerVO vo) {
		AwardManpowerResource awardManpowerResource = manpowerDao.getAwardManpowerResourceById(vo.getManpowerResourceId());
		awardManpowerResource.setIsResourceCreatedOrUpdated(Boolean.TRUE);
		updateAwardManpowerResourcePositionStatus(awardManpowerResource);
		if (awardManpowerResource.getPositionStatusCode() != null) {
			 awardManpowerResource.setManpowerPositionStatus(manpowerDao.getManpowerPositionStatusById(awardManpowerResource.getPositionStatusCode()));
		 }
		vo.setAwardManpowerResource(awardManpowerResource);
		return commonDao.convertObjectToJSON(vo);
	}

	private AwardManpowerResource updateAwardManpowerResourcePositionStatus(AwardManpowerResource awardManpowerResource) {
		awardManpowerResource.setPositionTriggerDate(commonDao.getCurrentTimestamp());
		awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_READY_TO_TRIGGER_POSITION);
		manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResource);
		return awardManpowerResource;
	}

	@Override
	public void saveWorkdayManpowerInterfaceDetails(Award newAward, Award activeAward, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, String activeAwardSuperiorSupOrg) throws Exception {
		String oldSuperiorSupOrg = null;
		String newSuperiorSupOrg = null;
		try {
			AtomicBoolean isProjectExtendedOrReduced = new AtomicBoolean(false);
			if (activeAward != null) {
				oldSuperiorSupOrg = activeAwardSuperiorSupOrg;
			}
			AtomicBoolean isSupOrgTriggerRequired = new AtomicBoolean(false);
			newSuperiorSupOrg = findSuperiorSupOrgForAward(newAward);
			List<String> editableSectionCodes = sectionWiseEditDao.getEditableSectionCodes(newAward.getAwardId().toString(), Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_MODULE_CODE, Constants.AWARD_SUBMODULE_CODE);
			if (editableSectionCodes != null && !editableSectionCodes.isEmpty() && (editableSectionCodes.contains(Constants.AWARD_KEY_PERSONNEL_EDITABLE_FIELD) || editableSectionCodes.contains(Constants.AWARD_CUSTOM_DATA_EDITABLE_FIELD))  && activeAward != null) {
				checkForThePIChangeOrSuperiorChange(newAward, activeAward, oldSuperiorSupOrg, newSuperiorSupOrg, newAwardPIPerson, activeAwardPIPerson, isSupOrgTriggerRequired);
			}
			List<AwardManpower> awardManpowerDetails = new ArrayList<>();
			if ((editableSectionCodes != null && !editableSectionCodes.isEmpty() && (editableSectionCodes.contains(Constants.MANPOWER_EDITABLE_FIELD) || editableSectionCodes.contains(Constants.MANPOWER_TRIGGER_RESOURCE_EDITABLE_FIELD))) || newAward.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) {
				isSupOrgTriggerRequired.set(true);
				awardManpowerDetails = manpowerDao.getAwardManpowerDetailsBasedTypeCodes(newAward.getAwardId(), Constants.MANPOWER_TYPE_STAFF);
			} else if (((editableSectionCodes != null && !editableSectionCodes.isEmpty() && editableSectionCodes.contains(Constants.AWARD_OVERVIEW_EDITABLE_FIELD)) || newAward.getAwardVariationTypeCode().equals(Constants.PROJECT_EXTENSION_SERVICE_REQUEST_TYPE_CODE)) && activeAward != null) {
				isProjectExtendedOrReduced.set(true);
				if (activeAward != null && !activeAward.getFinalExpirationDate().equals(newAward.getFinalExpirationDate())) {
					isSupOrgTriggerRequired.set(true);
				}
				awardManpowerDetails = manpowerDao.getAwardManpowerDetailsBasedTypeCodes(activeAward.getAwardId(), Constants.MANPOWER_TYPE_STAFF);
			}
			if (isSupOrgTriggerRequired.get()) {
				checkAndUpdatePreviousSupOrgEntryInQueue(newAward.getAwardId(), newAward.getAwardNumber(), Constants.MANPOWER_INTERFACE_CREATE_SUP_ORG);
			}
			if (awardManpowerDetails != null && !awardManpowerDetails.isEmpty()) {
				Set<Integer> manpowerIds = awardManpowerDetails.stream().map(AwardManpower::getAwardManpowerId).collect(Collectors.toSet());
				if (manpowerIds != null && !manpowerIds.isEmpty()) {
					List<AwardManpowerResource> awardManpowerResources = manpowerDao.getAllManpowerResources(manpowerIds);
					if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
						awardManpowerResources.stream().forEach(awardManpowerResource -> {
							try {
								if (isProjectExtendedOrReduced.get() && activeAward != null && !activeAward.getFinalExpirationDate().equals(newAward.getFinalExpirationDate()) && awardManpowerResource.getPositionOwnedByAward() != null && awardManpowerResource.getPositionOwnedByAward().equals("Y") && awardManpowerResource.getPositionId() != null) {
									Integer diff = newAward.getFinalExpirationDate().compareTo(activeAward.getFinalExpirationDate());
									checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION);
									setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), diff, newAward.getFinalExpirationDate(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION, awardManpowerResource.getFreezeDate(), null);
								}
								if ((manpowerDao.isManpowerSectionEditable(newAward.getAwardId().toString(),Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_MODULE_CODE,Constants.AWARD_SUBMODULE_CODE) || newAward.getAwardDocumentTypeCode().equals(Constants.AWARD_SETUP)) && (awardManpowerResource.getIsResourceCreatedOrUpdated() != null && Boolean.TRUE.equals(awardManpowerResource.getIsResourceCreatedOrUpdated()))) {
									if (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_PENDING_APPROVAL) || awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_POSITION_GENERATED) || awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION)) {
										List<String> costAllocationInterfaces = new ArrayList<>();
										costAllocationInterfaces.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION);
										costAllocationInterfaces.add(Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER);
										WorkdayManpowerInterface latestResourceAllocationInterface = manpowerIntegrationDao.getLatestResourceInterfaceByParams(awardManpowerResource.getResourceUniqueId(), costAllocationInterfaces);
										if (latestResourceAllocationInterface != null) {
											if (latestResourceAllocationInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_SUCCESS)) {
												setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, null, "N");
											} else if (latestResourceAllocationInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_PENDING) || latestResourceAllocationInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_ERROR)) {
												setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, latestResourceAllocationInterface.getInterfaceTypeCode(), null, latestResourceAllocationInterface.getIsCostAllocationCreate());
												latestResourceAllocationInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
												latestResourceAllocationInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
												manpowerDao.saveOrUpdateWorkdayManpowerInterface(latestResourceAllocationInterface);
											}
										} else if (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_PENDING_APPROVAL)) {
											setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION, null, "Y");
										} else if (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION)) {
											setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER, null, "Y");
											setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_HRBP, null, null);
										}
										if (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_HIRING_ON_EXISTING_POSITION)) {
											if (checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_HRBP)) {
												setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_HRBP, null, null);
											}
										} else if (awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_POSITION_GENERATED)) {
											if (checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_HRBP)) {
												setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_HRBP, null, null);
											}
											if (checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION)) {
												setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), 1, activeAward != null ? activeAward.getFinalExpirationDate() : newAward.getFinalExpirationDate(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION, awardManpowerResource.getFreezeDate(), null);
											}
										}
									} else if (awardManpowerResource.getCostAllocation() != null && awardManpowerResource.getCostAllocation().equals(new BigDecimal("100.00")) && awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_READY_TO_TRIGGER_POSITION)) {
										checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_POSITION_CREATION);
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_POSITION_CREATION, null, null);
										checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_HRBP);
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_HRBP, null, null);
										checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION);
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), 1, activeAward != null ? activeAward.getFinalExpirationDate() : newAward.getFinalExpirationDate(), Constants.MANPOWER_INTERFACE_FREEZE_POSITION, awardManpowerResource.getFreezeDate(), null);
										checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER);
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER, null, "Y");
									} else if (awardManpowerResource.getCostAllocation() != null && !awardManpowerResource.getCostAllocation().equals(new BigDecimal("100.00")) && awardManpowerResource.getPositionStatusCode().equals(Constants.MANPOWER_READY_TO_TRIGGER_POSITION)){
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_POSITION_CREATION, null, null);
										setAwardManpowerWorkdayDetails(awardManpowerResource, newAward.getAwardId(), null, null, Constants.MANPOWER_INTERFACE_COST_ALLOCATION_AFTER_MANPOWER, null, "Y");
									}
								}
							} catch (Exception e) {
								logger.error("Error in saveWorkdayManpowerInterfaceDetails : {}", e.getMessage());
								e.printStackTrace();
								throw new ApplicationException("Error in saveWorkdayManpowerInterfaceDetails", e, Constants.JAVA_ERROR);
							}
						});	
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in saveWorkdayManpowerInterfaceDetails : {}", e.getMessage());
			throw e;
		}
	}

	private void checkAndUpdatePreviousSupOrgEntryInQueue(Integer awardId, String awardNumber, String manpowerInterfaceType) throws Exception {
		try {
			List<String> costAllocationInterfaces = new ArrayList<>();
			costAllocationInterfaces.add(manpowerInterfaceType);
			WorkdayManpowerInterface latestResourceInterface = manpowerIntegrationDao.getLatestResourceInterfaceByAwardNumberAndInterfaceType(awardNumber, manpowerInterfaceType);
			if (latestResourceInterface != null) {
				if (latestResourceInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_ERROR) || latestResourceInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_PENDING)) {
					latestResourceInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
					latestResourceInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
					manpowerDao.saveOrUpdateWorkdayManpowerInterface(latestResourceInterface);
				}
			}
			WorkdayManpowerInterface manpowerInterface = new WorkdayManpowerInterface();
			manpowerInterface.setAwardId(awardId);
			manpowerInterface.setAwardNumber(awardNumber);
			manpowerInterface.setInterfaceTypeCode(manpowerInterfaceType);
			manpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_PENDING);
			manpowerInterface.setCreateUser("quickstart");
			manpowerInterface.setCreateTimestamp(commonDao.getCurrentTimestamp());
			manpowerInterface.setUpdateUser("quickstart");
			manpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpowerDao.saveOrUpdateWorkdayManpowerInterface(manpowerInterface);
		} catch (Exception e) {
			logger.error("Error in checkAndUpdatePreviousSupOrgEntryInQueue : {}", e.getMessage());
			throw e;
		}
	}

	private Boolean checkPreviousEntryInQueue(String resourceUniqueId, String manpowerInterfaceType) throws Exception {
		try {
			List<String> costAllocationInterfaces = new ArrayList<>();
			costAllocationInterfaces.add(manpowerInterfaceType);
			WorkdayManpowerInterface latestResourceInterface = manpowerIntegrationDao.getLatestResourceInterfaceByParams(resourceUniqueId, costAllocationInterfaces);
			if (latestResourceInterface != null) {
				if (latestResourceInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_ERROR) || latestResourceInterface.getInterfaceStatusCode().equals(Constants.MANPOWER_INTERFACE_PENDING)) {
					latestResourceInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_FEED_NOT_NEEDED);
					latestResourceInterface.setInterfaceTimestamp(commonDao.getCurrentTimestamp());
					manpowerDao.saveOrUpdateWorkdayManpowerInterface(latestResourceInterface);
					return true;
				}
			}
			return false;
		} catch (Exception e) {
			logger.error("Error in checkPreviousEntryInQueue : {}", e.getMessage());
			throw e;
		}
	}

	@Override
	public String findSuperiorSupOrgForAward(Award award) throws Exception {
		String unitNumber = null;
		String superiorSupOrgId = null;
		try {
			CustomDataElements customDataElement = customDataElementDao.fetchCustomDataElementDetail(Constants.CUSTOM_DATA_LEVEL_2_SUP_ORG);
			if (customDataElement != null) {
				CustomData customData = customDataElementDao.getCustomDataValue(Constants.MODULE_CODE_AWARD, award.getAwardId().toString(), customDataElement.getCustomElementId());
				if (customData != null) {
					unitNumber = customData.getValue() == null ? "" : customData.getValue();
					if (unitNumber != null && !unitNumber.equals("")) {
						superiorSupOrgId = manpowerIntegrationDao.getSuperiorSupervisoryOrganizationIdByUnitNumber(unitNumber);
					}
				}
			}
			if (unitNumber == null || unitNumber.equals("")) {
				superiorSupOrgId = manpowerIntegrationDao.getSuperiorSupervisoryOrganizationIdByUnitNumber(award.getLeadUnitNumber());
			}
		} catch (Exception e) {
			logger.info("Exception occured in findSuperiorSupOrgForAward : {}", e.getMessage());
			throw e;
		}
		return superiorSupOrgId;
	}

	private void checkForThePIChangeOrSuperiorChange(Award award, Award activeAward, String oldSuperiorSupOrg, String newSuperiorSupOrg, AwardPerson newAwardPIPerson, AwardPerson activeAwardPIPerson, AtomicBoolean isSupOrgTriggerRequired) throws Exception {
		try {
			if(activeAward != null && (newAwardPIPerson != null && activeAwardPIPerson != null && newAwardPIPerson.getPersonId() != null && activeAwardPIPerson.getPersonId() != null) && 
					((!newAwardPIPerson.getPersonId().equals(activeAwardPIPerson.getPersonId())) || (oldSuperiorSupOrg != null && newSuperiorSupOrg != null && !oldSuperiorSupOrg.equals(newSuperiorSupOrg)))) {
				    isSupOrgTriggerRequired.set(true);
					List<AwardManpowerResource> awardManpowerResources = manpowerDao.fetchAwardManpowerResourcesByAwardId(activeAward.getAwardId());
					String newPIPersonId = newAwardPIPerson.getPersonId();
					String oldPIPersonId = activeAwardPIPerson.getPersonId();
					awardManpowerResources.forEach(awardManpowerResource -> {
						try {
							checkPreviousEntryInQueue(awardManpowerResource.getResourceUniqueId(), Constants.MANPOWER_INTERFACE_MOVE_WORKERS);
							saveDetailsForPIChange(newPIPersonId, oldPIPersonId, award.getAwardNumber(), award.getAwardId(), oldSuperiorSupOrg, newSuperiorSupOrg, awardManpowerResource);
						} catch (Exception e) {
							logger.info("Exception in checkForThePIChangeOrSuperiorChange : {}", e.getMessage());
							e.printStackTrace();
						}
					});
			}	
		} catch (Exception e) {
			logger.info("Error in checkForThePIChangeOrSuperiorChange : {}", e.getMessage());
			throw e;
		}
	}

	private void saveDetailsForPIChange(String newPIPersonId, String oldPIPersonId, String awardNumber, Integer awardId, String oldSuperiorSupOrg, String newSuperiorSupOrg, AwardManpowerResource awardManpowerResource) {
		WorkdayManpowerInterface workdayManpowerInterface = new WorkdayManpowerInterface();
		workdayManpowerInterface.setAwardId(awardId);
		workdayManpowerInterface.setNewPIPersonId(newPIPersonId);
		workdayManpowerInterface.setOldPIPersonId(oldPIPersonId);
		workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_PENDING);
		workdayManpowerInterface.setInterfaceTypeCode(Constants.MANPOWER_INTERFACE_MOVE_WORKERS);
		workdayManpowerInterface.setCreateTimestamp(commonDao.getCurrentTimestamp());
		workdayManpowerInterface.setCreateUser("quickstart");
		workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		workdayManpowerInterface.setUpdateUser("quickstart");
		workdayManpowerInterface.setAwardNumber(awardNumber);
		workdayManpowerInterface.setOldSuperiorSupOrg(oldSuperiorSupOrg);
		workdayManpowerInterface.setNewSuperiorSupOrg(newSuperiorSupOrg);
		workdayManpowerInterface.setAwardManpowerId(awardManpowerResource.getAwardManpowerId());
		workdayManpowerInterface.setAwardManpowerResourceId(awardManpowerResource.getManpowerResourceId());
		workdayManpowerInterface.setAwardManpowerResource(awardManpowerResource);
		workdayManpowerInterface.setResourceUniqueId(awardManpowerResource.getResourceUniqueId());
		manpowerDao.saveOrUpdateWorkdayManpowerInterface(workdayManpowerInterface);
	}

	private WorkdayManpowerInterface setAwardManpowerWorkdayDetails(AwardManpowerResource awardManpowerResource, Integer awardId, Integer diff,Timestamp newAwardEndDate, String typeCode, Timestamp oldFreezeDate, String isCostAllocationCreate) throws Exception{
		WorkdayManpowerInterface workdayManpowerInterface = new WorkdayManpowerInterface();
		try {
			workdayManpowerInterface.setAwardId(awardId);
			workdayManpowerInterface.setAwardManpowerId(awardManpowerResource.getAwardManpowerId());
			workdayManpowerInterface.setAwardManpowerResourceId(awardManpowerResource.getManpowerResourceId());
			workdayManpowerInterface.setAwardManpowerResource(awardManpowerResource);
			workdayManpowerInterface.setInterfaceStatusCode(Constants.MANPOWER_INTERFACE_PENDING);
			workdayManpowerInterface.setInterfaceTypeCode(typeCode);
			workdayManpowerInterface.setCreateTimestamp(commonDao.getCurrentTimestamp());
			workdayManpowerInterface.setCreateUser("quickstart");
			workdayManpowerInterface.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			workdayManpowerInterface.setUpdateUser("quickstart");
			workdayManpowerInterface.setResourceUniqueId(awardManpowerResource.getResourceUniqueId());
			workdayManpowerInterface.setAwardNumber(awardManpowerResource.getAwardNumber());
			workdayManpowerInterface.setEndDateChange(diff);
			workdayManpowerInterface.setNewAwardEndDate(newAwardEndDate);
			workdayManpowerInterface.setOldFreezeDate(oldFreezeDate);
			workdayManpowerInterface.setIsCostAllocationCreate(isCostAllocationCreate);
			manpowerDao.saveOrUpdateWorkdayManpowerInterface(workdayManpowerInterface);
			
		} catch (Exception e) {
			logger.error("Error insetAwardManpowerWorkdayDetails : {} ", e.getMessage());
			throw e;
		}
		return workdayManpowerInterface;
	}

	@Override
	public String fetchManpowerPayrollDetails(ManpowerVO vo) {
		List<AwardManpowerPayroll> awardManpowerPayrolls = setAwardManpowerPayrollDetail(vo);
		vo.setAwardManpowerPayrolls(awardManpowerPayrolls);
		return commonDao.convertObjectToJSON(vo);
	}

	private List<AwardManpowerPayroll> setAwardManpowerPayrollDetail(ManpowerVO vo){
		List<AwardManpowerPayroll> awardManpowerPayrolls = manpowerDao.fetchManpowerPayrollDetails(vo.getInternalOrderCode(), vo.getEmployeeNumber());
		awardManpowerPayrolls.stream().forEach(awardManpowerPayroll -> {
			try {
				awardManpowerPayroll.setPayrollAmount(excelityService.decryptAESData(awardManpowerPayroll.getAmount()));
			} catch (Exception e) {
				logger.error("Error in decrpting the payroll amount: {} with error : {}", awardManpowerPayroll.getPayrollId(), e.getMessage());
			}
		});
		return awardManpowerPayrolls;
	}

	@Override
	public List<ManpowerPersonSearchResult> getPersonsWithPositionId(String searchString, Boolean isGraduateStudent, String awardId, String manpowerRequestType) {
		List<ManpowerPersonSearchResult> searchResult = new ArrayList<>();
		List<ManpowerPersonSearchResult> awardMapowerResources = new ArrayList<>();
		try {
			if(Boolean.FALSE.equals(isGraduateStudent)) {
				awardMapowerResources = manpowerDao.findManpowerPerson(Integer.parseInt(awardId), searchString, manpowerRequestType);
			} else {
				awardMapowerResources = manpowerDao.findGraduateStudents(searchString);
			}
			awardMapowerResources.stream().forEach(awardMapowerResource -> {
				if(awardMapowerResource.getFullName() != null || awardMapowerResource.getPersonId() != null
						|| awardMapowerResource.getPositionId() != null || awardMapowerResource.getPositionOwnedByAward() != null
						|| awardMapowerResource.getUserName() != null) {
					searchResult.add(awardMapowerResource);
				}
			});
			return searchResult;
		} catch (Exception e) {
			logger.info("Error ocuured in getPersonsWithPositionId {}", e.getMessage());
			return searchResult;
		}
	}

	@Override
	public List<ManpowerJobProfileType> getManpowerJobProfile(String searchString, String costElementCode) {
		return manpowerDao.getManpowerJobProfile(searchString, costElementCode);
	}

	@Override
	public BigDecimal calculateBudgetActualCommittedCost(Integer awardManpowerId, String manpowerTypeCode) {
		BigDecimal committedCost = BigDecimal.ZERO;
		if(!manpowerTypeCode.equals(Constants.MANPOWER_TYPE_OTHER)) {
			committedCost = manpowerDao.getSumOfCommittedAmount(awardManpowerId);
		}
		BigDecimal plannedSalary = manpowerDao.getSumOfPlannedSalary(awardManpowerId);
		return committedCost.add(plannedSalary);
	}
	
	@Override
	public String calculatePlannedSalary(ManpowerVO vo) {
		String multiplier = findMultiplier(vo.getAwardId());
		vo.setValidatedPlannedAmount(calculatePlannedCost(vo.getAwardManpowerResource(), vo.getAwardId().toString(), Boolean.FALSE, multiplier));
		vo.getAwardManpowerResource().setMultiplierValueUsed(new BigDecimal(multiplier));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void manpowerClosingPosition() {
		List<AwardManpowerResource> awardManpowerResources = manpowerDao.getActiveManpowerResources();
		if (awardManpowerResources != null && !awardManpowerResources.isEmpty()) {
			awardManpowerResources.forEach(awardManpowerResource -> {
				if (awardManpowerResource.getChargeEndDate() != null && (awardManpowerResource.getChargeEndDate().compareTo((commonDao.getCurrentTimestamp()))) < 0) {
					awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_ENDED);
				}
				if (awardManpowerResource.getPersonId() != null && !awardManpowerResource.getPersonId().equals("999999999100") && personDao.checkForPersonInactive(awardManpowerResource.getPersonId())) {
					awardManpowerResource.setPositionStatusCode(Constants.MANPOWER_EXPIRED);
				}
				manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResource);
			});
		}
		logger.info("Manpower expiring schedule ends at {}", commonDao.getCurrentTimestamp());
	}

	@Override
	public String findMultiplier(Integer awardId) {
		String multiplier = "1";
		CustomDataElements customDataElements = customDataElementDao.fetchCustomDataElementDetail(Constants.MANPOWER_RESOURCE_MULTIPLIER);
		if (customDataElements != null) {
			CustomData customData = customDataElementDao.getCustomDataValue(Constants.MODULE_CODE_AWARD, awardId.toString(), customDataElements.getCustomElementId());
			if (customData != null && customData.getValue() != null && !customData.getValue().equals("0") && !customData.getValue().equals("")) {
				multiplier = customData.getValue();
			} else {
				multiplier = findMultiplierFromParameter(multiplier);
			}
		} else {
			multiplier = findMultiplierFromParameter(multiplier);
		}
		return multiplier;
	}

	private String findMultiplierFromParameter(String multiplier) {
		String multiplierValue = manpowerIntegrationDao.getManpowerConfigurationValue(Constants.MANPOWER_RESOURCE_MULTIPLIER);
		if (multiplierValue != null && !multiplierValue.equals("") && !multiplierValue.equals("0")) {
			multiplier = multiplierValue;
		}
		return multiplier;
	}

	@Override
	public String overrideActualCommittedAmount(ManpowerVO vo) {
		AwardManpowerResource awardManpowerResource = manpowerDao.getAwardManpowerResourceById(vo.getManpowerResourceId());
		Manpower manpower = manpowerIntegrationDao.getManpowerByPersonId(awardManpowerResource.getPersonId());
		if (manpower != null && manpower.getBaseSalary() != null && vo.getActualCommittedAmount() != null) {
			AwardManpowerBaseSalaryHistory baseSalaryHistory = new AwardManpowerBaseSalaryHistory();
			baseSalaryHistory.setCurrentBaseSalary(manpower.getBaseSalary());
			manpowerIntegrationService.saveBaseSalaryHistory(baseSalaryHistory, awardManpowerResource, awardManpowerResource.getChargeStartDate(), awardManpowerResource.getChargeEndDate());
			awardManpowerResource.setBaseSalaryUsed(manpower.getBaseSalary());
		}
		awardManpowerResource.setCommittedCost(vo.getActualCommittedAmount());
		vo.setAwardManpowerResource(manpowerDao.saveOrUpdateAwardManpowerResources(awardManpowerResource));
		vo.setSapCommittedAmount(manpowerDao.getSumOfCommittedAmount(awardManpowerResource.getAwardManpowerId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public ResponseEntity<String> fetchManpowerBaseSalaryDetails(String awardNumber, String personId, String accountNumber) {
		if (Boolean.FALSE.equals(personDao.isPersonHasPermissionInAnyDepartment(AuthenticatedUser.getLoginPersonId(), Constants.MANPOWER_VIEW_BASE_SALARY_RIGHT_NAME))) {
			saveManpowerLogUserDetails(awardNumber, personId, accountNumber, Boolean.FALSE);
			return new ResponseEntity<>("Not Authorized to view the base salary details", HttpStatus.FORBIDDEN);
		}
		List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
		try {
			awardManpowerResources = manpowerDao.fetchManpowerBaseSalaryDetailsByAwardNumberAndPersonId(awardNumber, personId, accountNumber);
			String key = getDecryptedSecretKeyAES();
			Map<String, String> decryptedData = new HashMap<>();
			saveManpowerLogUserDetails(awardNumber, personId, accountNumber, Boolean.TRUE);
				awardManpowerResources.forEach(awardManpowerResource -> {
				if (awardManpowerResource.getBaseSalaryUsed() != null) {
					String encryptedData = awardManpowerResource.getBaseSalaryUsed().toString();
					if (decryptedData.get(encryptedData.toString()) != null) {
						awardManpowerResource.setBaseSalaryUsed(decryptedData.get(encryptedData));
					} else {
						try {
							decryptedData.put(encryptedData, decryptAESData(encryptedData, key));
						} catch (Exception e) {
							logger.error("Error occurred in fetchManpowerBaseSalaryDetails while decrypting data");
						}
						awardManpowerResource.setBaseSalaryUsed(decryptedData.get(encryptedData));
					}
				}
			});
		} catch (Exception e) {
			logger.error("Error occurred in fetchManpowerBaseSalaryDetails");
		}
		return new ResponseEntity<>(commonDao.convertObjectToJSON(awardManpowerResources), HttpStatus.OK);
	}

	private void saveManpowerLogUserDetails(String awardNumber, String personId, String accountNumber, Boolean accessStatus) {
		ManpowerLogUser manpowerLogUser = new ManpowerLogUser();
		manpowerLogUser.setAccountNumber(accountNumber);
		manpowerLogUser.setAwardNumber(awardNumber);
		manpowerLogUser.setPersonId(personId);
		manpowerLogUser.setLoginPersonId(AuthenticatedUser.getLoginPersonId());
		manpowerLogUser.setAccessStatus(accessStatus);
		manpowerDao.saveManpowerLogUserDetails(manpowerLogUser);
	}

	private String getDecryptedSecretKeyAES() throws Exception {
		return excelityService.decryptAESKey(excelityService.getSftpConfigurationValueAsString(Constants.AES_SECRETE_KEY));
	}

	private String decryptAESData(String data, String secretKey) throws Exception {
		try {
			byte[] iv = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			IvParameterSpec ivspec = new IvParameterSpec(iv);
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
			KeySpec spec = new PBEKeySpec(secretKey.toCharArray(), saltValue.getBytes(), 65536, 256);
			SecretKey tmp = factory.generateSecret(spec);
			SecretKeySpec secKey = new SecretKeySpec(tmp.getEncoded(), "AES");
			Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
			cipher.init(Cipher.DECRYPT_MODE, secKey, ivspec);
			return new String(cipher.doFinal(Base64.getDecoder().decode(data)));
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error while AES Data decrypting:", e.getMessage());
		}
		return null;
	}
  
	@Override
	public String fetchAwardManpowerForComparison(Integer awardId) {
		ManpowerVO vo = new ManpowerVO();
		vo.setAwardId(awardId);
		AwardBudgetHeader awardBudgetHeader = manpowerDao.getAwardBudgetByVersionNumber(vo.getAwardId());
		if (awardBudgetHeader != null && awardBudgetHeader.getBudgetId() != null) {
			vo.setBudgetVersionNumber(awardBudgetHeader.getVersionNumber());
			vo.setAwardBudgetHeaderId(awardBudgetHeader.getBudgetId());
		}
		vo.setManpowerCategory(prepareAwardManpowerDetails(vo));
		if (vo.getManpowerCategory() != null && vo.getManpowerCategory().getAwardManpowerDetails() != null && !vo.getManpowerCategory().getAwardManpowerDetails().isEmpty()) {
			for (Map.Entry<String, List<AwardManpower>> entry : vo.getManpowerCategory().getAwardManpowerDetails().entrySet()) {
	             for (AwardManpower manpower : entry.getValue()) {
	            	 manpower.setActualHeadCount(manpowerDao.countOfActiveManpowerResource(manpower.getAwardManpowerId()));
	             }
	        }
		}
		vo.setAccountNumber(awardDao.getAccountNumberByAwardId(vo.getAwardId()));
		return commonDao.convertObjectToJSON(vo);
	}

}
