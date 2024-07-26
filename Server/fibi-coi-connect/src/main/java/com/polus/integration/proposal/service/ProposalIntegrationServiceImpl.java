package com.polus.integration.proposal.service;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.appcorelib.questionnaire.service.QuestionnaireService;
import com.polus.integration.client.FcoiFeignClient;
import com.polus.integration.constant.Constant;
import com.polus.integration.dao.IntegrationDao;
import com.polus.integration.exception.service.MQRouterException;
import com.polus.integration.proposal.dao.ProposalIntegrationDao;
import com.polus.integration.proposal.dto.CoiDisclosureDTO;
import com.polus.integration.proposal.dto.ProposalDTO;
import com.polus.integration.proposal.dto.ProposalPersonDTO;
import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrQstnMapping;
import com.polus.integration.proposal.repository.ProposalQnAIntegrationRepository;
import com.polus.integration.proposal.vo.CreateProposalDisclosureVO;
import com.polus.integration.proposal.vo.ProcessProposalDisclosureVO;
import com.polus.integration.proposal.vo.QuestionnaireVO;
import com.polus.integration.proposal.vo.ValidateDisclosureVO;

import com.polus.integration.proposal.repository.ProposalIntegrationRepository;
import com.polus.integration.proposal.repository.ProposalPersonIntegrationRepository;


@Transactional
@Service
public class ProposalIntegrationServiceImpl implements ProposalIntegrationService {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationServiceImpl.class.getName());

	@Autowired
	private ProposalIntegrationRepository proposalIntegrationRepository;

	@Autowired
	private ProposalPersonIntegrationRepository proposalPersonIntegrationRepository;

	@Autowired
	private ProposalQnAIntegrationRepository qnAIntegrationRepository;

	@Autowired
	private IntegrationDao integrationDao;

	@Autowired
	private ProposalIntegrationDao proposalIntegrationDao;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private FcoiFeignClient fcoiFeignClient;

	@Value("${fibi.messageq.queues.devProposalIntegration}")
	private String devProposalIntegrationQueue;

	@Value("${fibi.messageq.queues.devPropQuesAnsIntegration}")
	private String devPropQuesAnsIntegrationQueue;

	private static final String ANSWERS = "ANSWERS";
	private static final String QUESTION_NUMBER = "QUESTION_NUMBER";
	private static final String AC_TYPE = "AC_TYPE";
	private static final String QUESTION_ID = "QUESTION_ID";

	@Override
	public void syncProposalDetails(ProposalDTO proposalDTO) {
		try {
			COIIntegrationProposal proposal = proposalIntegrationRepository.findProposalByProposalNumber(proposalDTO.getProposalNumber());
			if (proposal != null) {
				saveOrUpdateCOIProposalDetails(proposalDTO, proposal);
			} else {
				COIIntegrationProposal coiIntegrationProposal = new COIIntegrationProposal();
				coiIntegrationProposal.setFirstFedTimestamp(integrationDao.getCurrentTimestamp());
				saveOrUpdateCOIProposalDetails(proposalDTO, coiIntegrationProposal);
			}
			prepareProposalPersonDetail(proposalDTO);
		} catch (Exception e) {
	        logger.error("Error saving proposal details for ProposalDTO: {}", proposalDTO, e);
	        throw new MQRouterException("ER004", "Error saving proposal details for ProposalDTO: {}", e, e.getMessage(), devProposalIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE, Constant.DEV_PROPOSAL_MODULE_CODE, Constant.SUB_MODULE_CODE, Constant.PROPOSAL_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
	    }
	}

	private void saveOrUpdateCOIProposalDetails(ProposalDTO proposalDTO, COIIntegrationProposal coiIntegrationProposal) {
		coiIntegrationProposal.setProposalNumber(proposalDTO.getProposalNumber());
		coiIntegrationProposal.setIpNumber(proposalDTO.getIpNumber());
		coiIntegrationProposal.setSponsorGrantNumber(proposalDTO.getSponsorGrantNumber());
		coiIntegrationProposal.setVersionNumber(proposalDTO.getVersionNumber());
        coiIntegrationProposal.setProposalStartDate(proposalDTO.getStartDate());
        coiIntegrationProposal.setProposalEndDate(proposalDTO.getEndDate());
        coiIntegrationProposal.setSponsorCode(proposalDTO.getSponsorCode());
        coiIntegrationProposal.setSponsorName(proposalDTO.getSponsorName());
        coiIntegrationProposal.setPrimeSponsorCode(proposalDTO.getPrimeSponsorCode());
        coiIntegrationProposal.setPrimeSponsorName(proposalDTO.getPrimeSponsorName());
        coiIntegrationProposal.setLeadUnit(proposalDTO.getLeadUnit());
        coiIntegrationProposal.setLeadUnitName(proposalDTO.getLeadUnitName());
        coiIntegrationProposal.setProposalStatusCode(proposalDTO.getProposalStatusCode());
        coiIntegrationProposal.setProposalStatus(proposalDTO.getProposalStatus());
        coiIntegrationProposal.setProposalTypeCode(proposalDTO.getProposalTypeCode());
        coiIntegrationProposal.setProposalType(proposalDTO.getProposalType());
        coiIntegrationProposal.setTitle(proposalDTO.getTitle());
        coiIntegrationProposal.setLastFedTimestamp(integrationDao.getCurrentTimestamp());  
        coiIntegrationProposal.setDocumentUrl(proposalDTO.getDocumentUrl());
        coiIntegrationProposal.setSrcSysUpdateTimestamp(proposalDTO.getSrcSysUpdateTimestamp());
        coiIntegrationProposal.setSrcSysUpdateUsername(proposalDTO.getSrcSysUpdateUsername());
        coiIntegrationProposal.setAttribute1Label(proposalDTO.getAttribute1Label());
        coiIntegrationProposal.setAttribute1Value(proposalDTO.getAttribute1Value());
        coiIntegrationProposal.setAttribute2Label(proposalDTO.getAttribute2Label());
        coiIntegrationProposal.setAttribute2Value(proposalDTO.getAttribute2Value());
        coiIntegrationProposal.setAttribute3Label(proposalDTO.getAttribute3Label());
        coiIntegrationProposal.setAttribute3Value(proposalDTO.getAttribute3Value());
        coiIntegrationProposal.setAttribute4Label(proposalDTO.getAttribute4Label());
        coiIntegrationProposal.setAttribute4Value(proposalDTO.getAttribute4Value());
        coiIntegrationProposal.setAttribute5Label(proposalDTO.getAttribute5Label());
        coiIntegrationProposal.setAttribute5Value(proposalDTO.getAttribute5Value());
        proposalIntegrationDao.saveOrUpdateCoiIntegrationProposal(coiIntegrationProposal);
	}

	private void prepareProposalPersonDetail(ProposalDTO proposalDTO) {
		List<COIIntegrationProposalPerson> proposalPersons = proposalPersonIntegrationRepository.findProposalPersonsByProposalNumber(proposalDTO.getProposalNumber());
		saveOrUpdateProposalPersonDetail(proposalDTO.getProposalPersons(), proposalPersons);
	}

	private void saveOrUpdateProposalPersonDetail(List<ProposalPersonDTO> proposalPersonDTOs, List<COIIntegrationProposalPerson> proposalPersons) {
		proposalPersonDTOs.forEach(proposalPersonDTO -> {
		COIIntegrationProposalPerson proposalPerson = proposalPersons.stream()
	            .filter(person -> person.getKeyPersonId().equals(proposalPersonDTO.getKeyPersonId())
	            		&& person.getKeyPersonRole().equals(proposalPersonDTO.getKeyPersonRole())
	            				&& person.getProposalNumber().equals(proposalPersonDTO.getProposalNumber()))
	            .findFirst()
	            .orElse(new COIIntegrationProposalPerson());
	        preparePersonDetail(proposalPerson, proposalPersonDTO);
	        proposalIntegrationDao.saveOrUpdateCoiIntegrationProposalPerson(proposalPerson);
		});
	}

	private void preparePersonDetail(COIIntegrationProposalPerson coiIntegrationProposalPerson,	ProposalPersonDTO proposalPersonDTO) {
		if (coiIntegrationProposalPerson.getProposalNumber() == null) {
			coiIntegrationProposalPerson.setProposalNumber(proposalPersonDTO.getProposalNumber());
			coiIntegrationProposalPerson.setKeyPersonId(proposalPersonDTO.getKeyPersonId());
			coiIntegrationProposalPerson.setKeyPersonRoleCode(proposalPersonDTO.getKeyPersonRoleCode());
	        coiIntegrationProposalPerson.setKeyPersonRole(proposalPersonDTO.getKeyPersonRole());
		}
		coiIntegrationProposalPerson.setAttribute1Label(proposalPersonDTO.getAttribute1Label());
        coiIntegrationProposalPerson.setAttribute1Value(proposalPersonDTO.getAttribute1Value());
        coiIntegrationProposalPerson.setAttribute2Label(proposalPersonDTO.getAttribute2Label());
        coiIntegrationProposalPerson.setAttribute2Value(proposalPersonDTO.getAttribute2Value());
        coiIntegrationProposalPerson.setAttribute3Label(proposalPersonDTO.getAttribute3Label());
        coiIntegrationProposalPerson.setAttribute3Value(proposalPersonDTO.getAttribute3Value());
        coiIntegrationProposalPerson.setKeyPersonName(proposalPersonDTO.getKeyPersonName());
        coiIntegrationProposalPerson.setPercentageOfEffort(proposalPersonDTO.getPercentageOfEffort());
        coiIntegrationProposalPerson.setUpdateTimestamp(integrationDao.getCurrentTimestamp());
        proposalIntegrationDao.saveOrUpdateCoiIntegrationProposalPerson(coiIntegrationProposalPerson);
	}

	@Override
	public void syncPersonQuestionnaireAndCreateDisclosure(List<QuestionnaireVO> questionnaireVOs) {
		try {
			Set<Integer> proposalNumbers = new HashSet<>();
			questionnaireVOs.forEach(vo ->{
				if (!proposalNumbers.contains(vo.getProposalNumber())) {
					COIIntegrationProposal proposal = proposalIntegrationRepository.findProposalByProposalNumber(vo.getProposalNumber());
					if (proposal != null) {
						List<COIIntegrationPropQuestAns> questAnswers = qnAIntegrationRepository.findQuestionAnswersByProposalNumber(vo.getProposalNumber());
						saveOrUpdateQuestionAnswer(questAnswers, questionnaireVOs);
						proposalNumbers.add(vo.getProposalNumber());
						prepareValidateAndCreateDisclosureVO(vo);
					} else {
						logger.info("No such proposal exist in COIIntegrationProposal: {}", vo.getProposalNumber());
					}
				}
			});
		} catch (Exception e) {
			logger.error("Error in saving proposal questionnaire details: {}", questionnaireVOs, e);
			throw new MQRouterException("ER004", "Error in saving proposal questionnaire details: {}", e, e.getMessage(), devPropQuesAnsIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,  Constant.DEV_PROPOSAL_MODULE_CODE, Constant.SUB_MODULE_CODE, Constant.QUESTIONNAIRE_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
		}
	}

	private void saveOrUpdateQuestionAnswer(List<COIIntegrationPropQuestAns> questAnswers, List<QuestionnaireVO> questionnaireVOs) {
		questionnaireVOs.forEach(vo -> {
			COIIntegrationPropQuestAns integrationPropQuestAns = questAnswers.stream()
					.filter(questAnswer -> questAnswer.getKeyPersonId().equals(vo.getPersonId()) && 
							questAnswer.getQuestionId().equals(vo.getQuestionId())).findFirst()
					.orElse(new COIIntegrationPropQuestAns());
			prepareQuestionAnswer(integrationPropQuestAns, vo);
			proposalIntegrationDao.saveOrUpdateCoiIntegrationQuestionnaire(integrationPropQuestAns);
		});
	}

	private COIIntegrationPropQuestAns prepareQuestionAnswer(COIIntegrationPropQuestAns integrationPropQuestAns, QuestionnaireVO vo) {
		integrationPropQuestAns.setAnswer(vo.getAnswer());
		integrationPropQuestAns.setQuestionId(vo.getQuestionId());
		integrationPropQuestAns.setQuestionnaireId(vo.getQuestionnaireId());
		integrationPropQuestAns.setKeyPersonId(vo.getPersonId());
		integrationPropQuestAns.setProposalNumber(vo.getProposalNumber());
		integrationPropQuestAns.setUpdateTimestamp(integrationDao.getCurrentTimestamp());
		integrationPropQuestAns.setAttribute1Label(vo.getAttribute1Label());
		integrationPropQuestAns.setAttribute1Value(vo.getAttribute1Value());
		integrationPropQuestAns.setAttribute2Label(vo.getAttribute2Label());
		integrationPropQuestAns.setAttribute2Value(vo.getAttribute2Value());
		integrationPropQuestAns.setAttribute3Label(vo.getAttribute3Label());
		integrationPropQuestAns.setAttribute3Value(vo.getAttribute3Value());
		return integrationPropQuestAns;
	}

	private void prepareValidateAndCreateDisclosureVO(QuestionnaireVO vo) {
		ProcessProposalDisclosureVO processProposalDisclosureVO = new ProcessProposalDisclosureVO();
		processProposalDisclosureVO.setCoiProjectTypeCode(vo.getCoiProjectTypeCode());
		processProposalDisclosureVO.setHomeUnit(vo.getPersonHomeUnit());
		processProposalDisclosureVO.setModuleCode(Constant.DEV_PROPOSAL_MODULE_CODE.toString());
		processProposalDisclosureVO.setModuleItemId(vo.getProposalNumber());
		processProposalDisclosureVO.setPersonId(vo.getPersonId());
		validateAndCreateDisclosure(createValidateDisclosureVO(vo), processProposalDisclosureVO, vo.getQuestionnaireId());
	}

	private ValidateDisclosureVO createValidateDisclosureVO(QuestionnaireVO vo) {
		ValidateDisclosureVO disclosureVO = new ValidateDisclosureVO();
	    disclosureVO.setModuleCode(Constant.DEV_PROPOSAL_MODULE_CODE.toString());
	    disclosureVO.setModuleItemId(vo.getProposalNumber());
	    disclosureVO.setPersonId(vo.getPersonId());
	    return disclosureVO;
	}

	@SuppressWarnings("unchecked")
	public void validateAndCreateDisclosure(ValidateDisclosureVO validateDisclosureVO, ProcessProposalDisclosureVO vo, Integer questionnaireId) {			
		try {
			ResponseEntity<Object> response = fcoiFeignClient.validateDisclosure(validateDisclosureVO);
			Map<String, Object> responseObject = (Map<String, Object>) response.getBody();
			if (responseObject.get(Constant.PENDING_PROJECT) == null) {
				CreateProposalDisclosureVO disclosureVO = prepareCreateProposalDisclosureResponse(vo);
				ResponseEntity<Object> responseObj = fcoiFeignClient.createDisclosure(disclosureVO);
				Map<String, Object> responseBody = (Map<String, Object>) responseObj.getBody();
				Map<String, Object> coiDisclosure = (Map<String, Object>) responseBody.get("coiDisclosure");
				logger.info("Disclosure created successfully.");
				syncQuestionniareAnswers(coiDisclosure, vo, questionnaireId);
			} else {
				logger.info("Pending project exists, disclosure creation skipped.");
				Map<String, Object> coiDisclosure = (Map<String, Object>) responseObject.get(Constant.PENDING_PROJECT);
				syncQuestionniareAnswers(coiDisclosure, vo, questionnaireId);
            }
		} catch (Exception e) {
			logger.error("Exception occurred while validating or creating disclosure", e);
			throw new MQRouterException("ER004", "Error in validateAndCreateDisclosure: {}", e, e.getMessage(), devPropQuesAnsIntegrationQueue, null, Constant.FIBI_DIRECT_EXCHANGE,  Constant.DEV_PROPOSAL_MODULE_CODE, Constant.SUB_MODULE_CODE, Constant.QUESTIONNAIRE_INTEGRATION_ACTION_TYPE, integrationDao.generateUUID());
		}
    }

	private void syncQuestionniareAnswers(Map<String, Object> coiDisclosure, ProcessProposalDisclosureVO vo, Integer questionnaireId) {
		QuestionnaireVO questionnaireVO = new QuestionnaireVO();
		questionnaireVO.setProposalNumber(vo.getModuleItemId());
		questionnaireVO.setPersonId(vo.getPersonId());
		questionnaireVO.setQuestionnaireId(questionnaireId);
		questionnaireVO.setDisclosureId((Integer) coiDisclosure.get("disclosureId"));
		getQuestionnaire(questionnaireVO);
	}

	private CreateProposalDisclosureVO prepareCreateProposalDisclosureResponse(ProcessProposalDisclosureVO vo) {
		CreateProposalDisclosureVO disclosureVO = new CreateProposalDisclosureVO();
	    CoiDisclosureDTO coiDisclosureDTO = new CoiDisclosureDTO();
	    coiDisclosureDTO.setCoiProjectTypeCode(vo.getCoiProjectTypeCode());
	    coiDisclosureDTO.setHomeUnit(vo.getHomeUnit());
	    coiDisclosureDTO.setModuleItemKey(vo.getModuleItemId());
	    coiDisclosureDTO.setPersonId(vo.getPersonId());
	    disclosureVO.setCoiDisclosure(coiDisclosureDTO);
	    disclosureVO.setPersonId(vo.getPersonId());
	    return disclosureVO;
	}

	
	private String getQuestionnaire(QuestionnaireVO vo) {
		try {
			FibiCoiQnrMapping  qnrMapping = proposalIntegrationDao.getQuestionnaireMappingInfo(vo.getQuestionnaireId());
			QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
			questionnaireDataBus.setQuestionnaireId(qnrMapping.getFibiQnrId());
			questionnaireDataBus = questionnaireService.getQuestionnaireDetails(questionnaireDataBus);
			questionnaireDataBus = saveQuestionnaireAnswers(questionnaireDataBus, qnrMapping.getQuestions(), vo.getPersonId(), vo.getProposalNumber(), vo.getDisclosureId(), vo.getQuestionnaireId());
			return integrationDao.convertObjectToJSON(questionnaireDataBus);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception occurred while getQuestionnaire", e);
			throw new MQRouterException("ER004", "Error in getQuestionnaire: {}", e, e.getMessage(), "saveQuestionnaire", null, Constant.FIBI_DIRECT_EXCHANGE,  Constant.COI_MODULE_CODE, Constant.SUB_MODULE_CODE, "saveQuestionnaire", integrationDao.generateUUID());
		}
	}

	public QuestionnaireDataBus saveQuestionnaireAnswers(QuestionnaireDataBus questionnaireDataBus, List<FibiCoiQnrQstnMapping> mappingQuestions, String disclosurePersonId, Integer proposalNumber, Integer disclosureId, Integer questionnaireId) {
		try {
			questionnaireDataBus.setModuleItemCode(Constant.COI_MODULE_CODE);
			questionnaireDataBus.setModuleSubItemCode(Constant.SUB_MODULE_CODE);
			questionnaireDataBus.setModuleSubItemKey(Constant.SUB_MODULE_ITEM_KEY);
			questionnaireDataBus.setModuleItemKey(disclosureId.toString());
			questionnaireDataBus.setQuestionnaireCompleteFlag("Y");
			if (questionnaireDataBus.getQuestionnaire() != null && questionnaireDataBus.getQuestionnaire().getQuestions() != null) {
				List<HashMap<String, Object>> fibiQuestions = questionnaireDataBus.getQuestionnaire().getQuestions();
				prepareQuestionAnswers(fibiQuestions, mappingQuestions, questionnaireId, proposalNumber, disclosurePersonId);
			}
			return questionnaireService.saveQuestionnaireAnswers(questionnaireDataBus, null);
		} catch (Exception e) {
			logger.error("Exception occurred while saveQuestionnaire", e);
			throw new MQRouterException("ER004", "Error in saveQuestionnaire: {}", e, e.getMessage(), "saveQuestionnaire", null, Constant.FIBI_DIRECT_EXCHANGE,  Constant.DEV_PROPOSAL_MODULE_CODE, Constant.SUB_MODULE_CODE, "saveQuestionnaire", integrationDao.generateUUID());
		}
	}

	@SuppressWarnings("unchecked")
	private void prepareQuestionAnswers(List<HashMap<String, Object>> questions, List<FibiCoiQnrQstnMapping> mappingQuestions, Integer questionnaireId, Integer proposalNumber, String disclosurePersonId) {
		questions.forEach(question -> {
			mappingQuestions.forEach(mappingQuestion -> {
				if (question.get(QUESTION_NUMBER).equals(mappingQuestion.getFibiQstnNum()) && question.get(QUESTION_ID).equals(mappingQuestion.getFibiQstnId())) {
					question.put(AC_TYPE, "I");
					String answer = proposalIntegrationDao.getQuestionAnswerByParams(mappingQuestion.getSourceQstnId(), questionnaireId, proposalNumber, disclosurePersonId);
					HashMap<String, String> answers = new HashMap<>();
					if (question.get(ANSWERS) != null) {
						answers = (HashMap<String, String>) question.get(ANSWERS);
					}
					answers.put("1", answer);
				}
			});
		});
	}

}