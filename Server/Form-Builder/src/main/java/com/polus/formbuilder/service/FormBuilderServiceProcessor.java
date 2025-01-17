package com.polus.formbuilder.service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.appcorelib.authentication.AuthenticatedUser;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.appcorelib.questionnaire.service.QuestionnaireService;
import com.polus.formbuilder.customdataelement.VO.CustomDataElementVO;
import com.polus.formbuilder.customdataelement.service.CustomDataElementService;
import com.polus.formbuilder.dao.FormBuilderServiceProcessorDAO;
import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;
import com.polus.formbuilder.dto.FormBuilderSectionsDTO;
import com.polus.formbuilder.dto.FormResponseDTO;
import com.polus.formbuilder.entity.FormBuilderHeaderEntity;
import com.polus.formbuilder.entity.FormBuilderProgElementEntity;
import com.polus.formbuilder.entity.FormBuilderSectionComponentEntity;
import com.polus.formbuilder.entity.FormBuilderSectionEntity;
import com.polus.formbuilder.entity.FormBuilderUsageEntity;
import com.polus.formbuilder.model.ApplicableFormRequest;
import com.polus.formbuilder.model.ApplicableFormResponse;
import com.polus.formbuilder.model.BlankFormRequest;
import com.polus.formbuilder.model.BlankFormResponse;
import com.polus.formbuilder.model.FormComponentFetchRequest;
import com.polus.formbuilder.model.FormComponentFetchResponse;
import com.polus.formbuilder.model.FormComponentSaveRequest;
import com.polus.formbuilder.model.FormComponentSaveResponse;
import com.polus.formbuilder.model.FormEvaluateValidationResponse;
import com.polus.formbuilder.model.FormRequest;
import com.polus.formbuilder.model.FormResponse;
import com.polus.formbuilder.model.FormValidationRequest;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;
import com.polus.formbuilder.programmedelement.ProgrammedElementService;
import com.polus.formbuilder.repository.FormBuilderHeaderEntityRepository;
import com.polus.formbuilder.repository.FormBuilderProgElementEntityRepository;
import com.polus.formbuilder.repository.FormBuilderSectionComponentEntityRepository;
import com.polus.formbuilder.repository.FormBuilderSectionEntityRepository;
import com.polus.formbuilder.repository.FormBuilderUsageEntityRepository;

@Service
public class FormBuilderServiceProcessor {

	@Autowired
	private ProgrammedElementService programmedElementService;

	@Autowired
	private QuestionnaireService questionnaireService;
	
	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	private FormBuilderHeaderEntityRepository fmHeaderRepository;
	
	@Autowired
	private FormBuilderSectionEntityRepository sectionRepository;
	
	@Autowired
	private FormBuilderSectionComponentEntityRepository componentRepository;
	
	@Autowired
	private FormBuilderProgElementEntityRepository programmedElementRepository;
	
	@Autowired
	private FormBuilderServiceProcessorDAO formDAO;

	@Autowired
	private FormBuilderUsageEntityRepository usageRepository;

	public ApplicableFormResponse PerformGetApplicableForms(ApplicableFormRequest request) {

//		List<Integer> applicableFormId = formDAO.getApplicableFormIds(request.getModuleItemCode(),
//				request.getModuleSubItemCode(), request.getDocumentOwnerPersonId());
//
//		Integer primaryFormID = getPrimaryFormId(applicableFormId);
//		
//		var response = ApplicableFormResponse.builder()
//										.applicableFormsBuilderIds(applicableFormId)
//										.formsBuilderId(primaryFormID)
//										.build();
//
//		return response;
		
		List<FormBuilderUsageEntity> allApplicableForms = usageRepository.fetchByModuleAndSubModule(request.getModuleItemCode(), request.getModuleSubItemCode());

		List<FormBuilderUsageEntity> filteredApplicableForms = formDAO.evaluateFormRule(allApplicableForms,null, request.getModuleItemCode(), request.getModuleSubItemCode(),request.getDocumentOwnerPersonId(),  null, null);

		List<Integer> applicableFormId = new ArrayList<>();
		filteredApplicableForms.forEach(form -> applicableFormId.add(form.getFormBuilderId()));
		Integer primaryFormID = getPrimaryFormId(applicableFormId);
		
		var response = ApplicableFormResponse.builder()
										.applicableFormsBuilderIds(applicableFormId)
										.formsBuilderId(primaryFormID)
										.build();

		return response;
			
		}

	public BlankFormResponse PerformGetBankFormbyModule(BlankFormRequest request) {
		
		List<Integer> applicableFormId = formDAO.getApplicableFormIds(request.getModuleItemCode(),
																	  request.getModuleSubItemCode(),
																	  request.getDocumentOwnerPersonId());
		
		Integer primaryFormID = getPrimaryFormId(applicableFormId);
		
		
		FormResponseDTO formResponseDTO = getFormData(primaryFormID,
													  request.getModuleItemCode(),
													  request.getModuleSubItemCode(),
													  request.getModuleItemKey(),
													  request.getModuleSubItemKey(),
													  request.getDocumentOwnerPersonId());	
		
		var response = BlankFormResponse.builder()
										.form(formResponseDTO)
										.formsBuilderId(primaryFormID)
										.applicableFormsBuilderIds(applicableFormId).build();

		return response;

	}
	

	public BlankFormResponse PerformGetBankFormbyFormId(BlankFormRequest request) {
		
		FormResponseDTO formResponseDTO = getFormData(request.getFormBuilderId(),
													  request.getModuleItemCode(),
													  request.getModuleSubItemCode(),
													  request.getModuleItemKey(),
													  request.getModuleSubItemKey(),
													  request.getDocumentOwnerPersonId());	
		
		var response = BlankFormResponse.builder()
										.form(formResponseDTO)
										.build();

		return response;
	}
	
	public FormResponse PerformGetForm(FormRequest request) {
		
		FormResponseDTO formResponseDTO =   getFormData(request.getFormBuilderId(),
													  	request.getModuleItemCode(),
													  	request.getModuleSubItemCode(),
													  	request.getModuleItemKey(),
													  	request.getModuleSubItemKey(),
													  	request.getDocumentOwnerPersonId()
													  	);
		var response =  FormResponse.builder()
									.form(formResponseDTO)
									.build();
		return response;
		
	}
	
	public FormResponse PerformGetFormSection(FormRequest request) {
		
		return FormResponse.builder()
						   .form(getSectionData(request.getFormBuilderId(),
								   				request.getModuleItemCode(),
								   				request.getModuleSubItemCode(),
								   				request.getModuleItemKey(),
								   				request.getModuleSubItemKey()))
						   .build();
	}
	
	public FormComponentFetchResponse PerformGetQuestionnaireComponent(FormComponentFetchRequest request) {
		
		// component Id is saved as moduleSubItemKey in the Form Builder module
		// for Questionnaire Engine and Custom Element Engine
		FormBuilderSectionsComponentDTO componentDTO = getComponentInfoById(request.getComponentId());
		componentDTO.setQuestionnaire(		
										getQuestionnaireComponent(request.getModuleItemCode(),
																  request.getModuleSubItemCode(),
																  request.getModuleItemKey(),
																  request.getComponentId().toString(),
																  request.getComponentRefId())
									 );
		return (FormComponentFetchResponse) componentDTO;
	}


	public FormComponentFetchResponse PerformGetCustomElementComponent(FormComponentFetchRequest request) {
		
		// component Id is saved as moduleSubItemKey in the Form Builder module
		// for Questionnaire Engine and Custom Element Engine
		
		FormBuilderSectionsComponentDTO componentDTO = getComponentInfoById(request.getComponentId());
		componentDTO.setCustomElement(
										getCustomElementComponent(request.getModuleItemCode(),
																  request.getModuleSubItemCode(),
																  request.getModuleItemKey(),
																  request.getComponentId().toString(),
																  request.getComponentRefId())				
									  
									);
		return (FormComponentFetchResponse) componentDTO;
	}
	
	public FormComponentFetchResponse PerformGetProgrammedElementComponent(FormComponentFetchRequest request) {
		
		Integer id = Integer.parseInt(request.getComponentRefId());
		FormBuilderProgElementEntity programmedElementEntity = fetchProgrammedElementbyId(id);
		
		String programmedElementName = programmedElementEntity.getProgElementName();		
		
		CompletableFuture<ProgrammedElementModel> PEResponseFuture = CompletableFuture
				.supplyAsync(() -> programmedElementService.getBlankResponse(programmedElementName));

		CompletableFuture<FormBuilderSectionsComponentDTO> componentFuture = CompletableFuture
				.supplyAsync(() -> getComponentInfoById(request.getComponentId()));

		CompletableFuture<Void> allOf = CompletableFuture.allOf(PEResponseFuture, componentFuture);
		
		try {
			allOf.get(); 
		} catch (InterruptedException | ExecutionException e) {
			e.printStackTrace(); 
		}
		
		ProgrammedElementModel blankResponse = PEResponseFuture.join();
		FormBuilderSectionsComponentDTO componentDTO = componentFuture.join();
		componentDTO.setProgrammedElement(blankResponse);
		
		return (FormComponentFetchResponse) componentDTO;
	}


	
	public FormComponentSaveResponse 
					PerformSaveQuestionnaireComponent(FormComponentSaveRequest request, 
													  MultipartHttpServletRequest multipartRequest) {
	
		// component Id is saved as moduleSubItemKey in the Form Builder module
		// for Questionnaire Engine and Custom Element Engine
		QuestionnaireDataBus questionnaireBus = request.getQuestionnaire();
		questionnaireBus.setModuleItemCode(Integer.parseInt(request.getModuleItemCode()));
		questionnaireBus.setModuleSubItemCode(Integer.parseInt(request.getModuleSubItemCode()));
		questionnaireBus.setModuleItemKey(request.getModuleItemKey());
		questionnaireBus.setModuleSubItemKey(request.getComponentId().toString());
		questionnaireBus.setQuestionnaireCompleteFlag(request.getQuestionnaire().getQuestionnaireCompleteFlag());
		
		questionnaireBus = questionnaireService.saveQuestionnaireAnswers(questionnaireBus, multipartRequest);		
		var response = initialComponentSaveReponse(request);
		response.setQuestionnaire(questionnaireBus);
		
		return response;
	}


	public FormComponentSaveResponse PerformSaveCustomElementComponent(FormComponentSaveRequest request) {
		
		CustomDataElementVO customElement = request.getCustomElement();			
		// component Id is saved as moduleSubItemKey in the Form Builder module
		// for Questionnaire Engine and Custom Element Engine
		customElement.setModuleCode(Integer.parseInt(request.getModuleItemCode()));
		customElement.setSubModuleCode(Integer.parseInt(request.getModuleSubItemCode()));
		customElement.setModuleItemKey(request.getModuleItemKey());
		customElement.setModuleSubItemKey(String.valueOf(request.getComponentId()));
		
		customElement = customDataElementService.saveCustomResponse(customElement);
		FormBuilderSectionsComponentDTO componentDTO = getComponentInfoById(request.getComponentId());
		var response = initialComponentSaveReponse(request);
		response.setCustomElement(customElement);
		
		return response;
		
	}
	
	public FormComponentSaveResponse PerformSaveProgrammedElementComponent(FormComponentSaveRequest request) {
		
		String programmedElementName = request.getComponentData();
		if(programmedElementName == null) {
			Optional<FormBuilderProgElementEntity> programmedElementOptional = programmedElementRepository.findById(Integer.parseInt(request.getComponentRefId()));
			if(programmedElementOptional.isEmpty()) {
				throw new RuntimeException("No entry in Programmed Element for Id "+request.getComponentRefId());
			}
			
			FormBuilderProgElementEntity programmedElementEntity = programmedElementOptional.get();
			programmedElementName = programmedElementEntity.getProgElementName();
		}
			
		var moduleDetails = 
				 ProgrammedElementModuleDetails.builder()
				 							   .moduleItemCode(request.getModuleItemCode())
				 							   .moduleSubItemCode(request.getModuleSubItemCode())
				 							   .moduleItemKey(request.getModuleItemKey())
				 							   .moduleSubItemKey(request.getModuleSubItemKey())
				 							   .loggedInUser(getLoggedInUser())
				 							   .build();
		
		ProgrammedElementModel programmedElement =  programmedElementService.save(programmedElementName,moduleDetails, request.getProgrammedElement());
		var response = initialComponentSaveReponse(request);
		response.setProgrammedElement(programmedElement);
		
		return response;
	}
	

	private FormResponseDTO getFormData(Integer formID,
										String moduleItemCode,
										String moduleSubItemCode,
										String moduleItemKey,
										String moduleSubItemKey,
										String documentOwnerPersonId) {
		Instant start = Instant.now();
		
		//Prepare task to run parallel		
		CompletableFuture<List<FormBuilderSectionsComponentDTO>> componentListFuture = CompletableFuture
				.supplyAsync(() -> formDAO.getComponentsForFormId(formID));

		CompletableFuture<List<FormBuilderSectionEntity>> sectionListFuture = CompletableFuture
				.supplyAsync(() -> sectionRepository.getSectionDetailsByFormId(formID));

		CompletableFuture<FormBuilderHeaderEntity> formHeaderDetailsFuture = CompletableFuture
				.supplyAsync(() -> fmHeaderRepository.findById(formID).get());

		CompletableFuture<Void> allOf = CompletableFuture.allOf(componentListFuture, sectionListFuture,
				formHeaderDetailsFuture);
		
		try {
			allOf.get(); // Wait for all above tasks to complete
		} catch (InterruptedException | ExecutionException e) {
			e.printStackTrace(); 
		}

		// Retrieve the results
		List<FormBuilderSectionsComponentDTO> componentList = componentListFuture.join();
		List<FormBuilderSectionEntity> sectionList = sectionListFuture.join();
		FormBuilderHeaderEntity formHeaderDetails = formHeaderDetailsFuture.join();

		// Process componentList
		componentList = processComponentType(componentList,
											 moduleItemCode,
											 moduleSubItemCode,
											 moduleItemKey,
											 moduleSubItemKey);
		

		// Process sectionList
		Map<Integer, List<FormBuilderSectionsComponentDTO>> SectionComponentGrouping = componentList.parallelStream()
				.collect(Collectors.groupingBy(FormBuilderSectionsComponentDTO::getSectionId));

		//Map Entity to DTO
		List<FormBuilderSectionsDTO> sectionDTOList = sectionList.parallelStream()
				 .map( e -> mapSectionEntityToDTO(e,SectionComponentGrouping))
                .collect(Collectors.toList());
		
		// Create FormResponseDTO
		FormResponseDTO formResponseDTO = new FormResponseDTO();
		formResponseDTO.setFormBuilderId(formHeaderDetails.getFormBuilderId());
		formResponseDTO.setFormBuilderNumber(formHeaderDetails.getFormBuilderNumber());
		formResponseDTO.setModuleItemCode(moduleItemCode);
		formResponseDTO.setModuleSubItemCode(moduleSubItemCode);
		formResponseDTO.setModuleItemKey(moduleItemKey);
		formResponseDTO.setModuleSubItemKey(moduleSubItemKey);
		formResponseDTO.setFormName(formHeaderDetails.getDescription());
		formResponseDTO.setFormSections(sectionDTOList);
		
		formResponseDTO.setDisabledSections(
							fetchDisabledSection(formHeaderDetails.getFormBuilderId(),
												 moduleItemCode,
												 moduleSubItemCode,
												 moduleItemKey,
												 moduleSubItemKey,
												 documentOwnerPersonId));
		
		
		return formResponseDTO;
	}


	private FormResponseDTO getSectionData( Integer sectionId,
											String moduleItemCode,
											String moduleSubItemCode,
											String moduleItemKey,
											String moduleSubItemKey) {
		Instant start = Instant.now();
		
		CompletableFuture<List<FormBuilderSectionComponentEntity>> componentListFuture = CompletableFuture
				.supplyAsync(() -> componentRepository.getAllComponentBySection(sectionId));

		CompletableFuture<List<FormBuilderSectionEntity>> sectionListFuture = CompletableFuture
				.supplyAsync(() -> sectionRepository.getSectionDetailsByFormId(sectionId));

		CompletableFuture<Void> allOf = CompletableFuture.allOf(componentListFuture, sectionListFuture);

		try {
			allOf.get(); // Wait for all above tasks to complete
		} catch (InterruptedException | ExecutionException e) {
			e.printStackTrace(); 
		}

		// Retrieve the results
		List<FormBuilderSectionComponentEntity> componentEntityList = componentListFuture.join();		
		List<FormBuilderSectionEntity> sectionList = sectionListFuture.join();
		
		
		//Mapping ComponentEntity to ComponentDTO
		List<FormBuilderSectionsComponentDTO> componentList = componentEntityList
																	.parallelStream()
																	.map( e -> mapComponentEntityToDTO(e))
																	.collect(Collectors.toList());

		// Process componentList in parallel
		
		componentList = processComponentType(componentList,
											 moduleItemCode,
											 moduleSubItemCode,
											 moduleItemKey,
											 moduleSubItemKey);
		
		
		// Process sectionList in parallel
		Map<Integer, List<FormBuilderSectionsComponentDTO>> SectionComponentGrouping = componentList.parallelStream()
				.collect(Collectors.groupingBy(FormBuilderSectionsComponentDTO::getSectionId));

		
		List<FormBuilderSectionsDTO> sectionDTOList = sectionList.parallelStream()
																 .map( e -> mapSectionEntityToDTO(e,SectionComponentGrouping))
												                 .collect(Collectors.toList());
		
		
		// Create FormResponseDTO
		FormResponseDTO formResponseDTO = new FormResponseDTO();
		
		formResponseDTO.setFormSections(sectionDTOList);	
		
		return formResponseDTO;
	}
	
	private List<FormBuilderSectionsComponentDTO> processComponentType(
															List<FormBuilderSectionsComponentDTO> componentList,
															String moduleItemCode,
															String moduleSubItemCode,
															String moduleItemKey,
															String moduleSubItemKey
															) {		
		
		// component Id is saved as moduleSubItemKey in the Form Builder module
		// for Questionnaire Engine and Custom Element Engine
		
		componentList.stream()
					.forEach(component -> {
						
							if (component.getComponentType().equals(FormBuilderConstants.QUESTIONNAIR_COMPONENT)) {
									if(component.getComponentRefId()!=null && !component.getComponentRefId().trim().isEmpty()) {
									component.setQuestionnaire(
															getQuestionnaireComponent(moduleItemCode,
																					  moduleSubItemCode, 
																					  moduleItemKey,
																					  component.getComponentId().toString(), 
																					  component.getComponentRefId()));
									}
									
									
							} else if (FormBuilderConstants.CUSTOM_ELEMENT_COMPONENT_LIST.contains(component.getComponentType())) {
								if(component.getComponentRefId()!=null && !component.getComponentRefId().trim().isEmpty()) {
									component.setCustomElement(
															getCustomElementComponent(moduleItemCode,
																					  moduleSubItemCode,
																					  moduleItemKey,
																					  component.getComponentId().toString(),
																					  component.getComponentRefId()));
								}
							} else if (component.getComponentType().equals(FormBuilderConstants.PROGRAMMED_ELEMENT_COMPONENT)) {
								
								if(component.getComponentRefId()!=null && !component.getComponentRefId().trim().isEmpty()) {
									component.setProgrammedElement(
															getProgrammedElementComponent(moduleItemCode,
																					  moduleSubItemCode,
																					  moduleItemKey,
																					  moduleSubItemKey,																					  
																					  component.getComponentRefId(),
																					  component.getProgrammedElement()
																					  ));
								}
								
								
							}
							
					});		
		
		return componentList;
	}


	private CustomDataElementVO getCustomElementComponent(String moduleItemCode,
														  String moduleSubItemCode,
														  String moduleItemKey,
														  String moduleSubItemKey,
														  String customElementId) {

		CustomDataElementVO customDataElementVO = new CustomDataElementVO();
		customDataElementVO.setModuleCode(Integer.parseInt(moduleItemCode));
		customDataElementVO.setModuleItemKey(moduleItemKey);
		customDataElementVO.setModuleSubItemKey(moduleSubItemKey);
		customDataElementVO.setSubModuleCode(Integer.parseInt(moduleSubItemCode));
		customDataElementVO.setCustomDataElementId(Integer.parseInt(customElementId));
		customDataElementVO = customDataElementService.fetchCustomElementById(customDataElementVO);
		return customDataElementVO;

	}

	private ProgrammedElementModel getProgrammedElementComponent(String moduleItemCode,
															  String moduleSubItemCode,
															  String moduleItemKey,
															  String moduleSubItemKey,
															  String programmedElementId,
															  ProgrammedElementModel request) {

		
		Integer id = Integer.parseInt(programmedElementId);
		FormBuilderProgElementEntity programmedElementEntity = fetchProgrammedElementbyId(id);
		
		if (moduleSubItemKey == null) {
					return	programmedElementService.getBlankResponse(programmedElementEntity.getProgElementName());
		}
		 var moduleDetails = 
				 ProgrammedElementModuleDetails.builder()
				 							   .moduleItemCode(moduleItemCode)
				 							   .moduleSubItemCode(moduleSubItemCode)
				 							   .moduleItemKey(moduleItemKey)
				 							   .moduleSubItemKey(moduleSubItemKey)
				 							   .loggedInUser(getLoggedInUser())
				 							   .build();
		 
		return	programmedElementService.getResponse(programmedElementEntity.getProgElementName(),
													 moduleDetails,
													 request
													 );
	}
	
	private QuestionnaireDataBus getQuestionnaireComponent( String moduleItemCode,
															String moduleSubItemCode,
															String moduleItemKey,
															String moduleSubItemKey, 
															String questionnaireId) {
		
		return questionnaireService.getQuestionnaireDetails(
											 intialQuestionnaireDataBusObject(Integer.parseInt(questionnaireId),
																				 moduleItemCode,
																				 moduleSubItemCode,
																				 moduleItemKey,
																				 moduleSubItemKey								
																	 		));
	}


	private QuestionnaireDataBus intialQuestionnaireDataBusObject(Integer qnrId,
																  String moduleItemCode,
																  String moduleSubItemCode,
																  String moduleItemKey,
																  String moduleSubItemKey) {
	   	QuestionnaireDataBus bus = new QuestionnaireDataBus();
		bus.setQuestionnaireId(qnrId);
		bus.setModuleItemCode(Integer.parseInt(moduleItemCode));
		bus.setModuleSubItemCode(Integer.parseInt(moduleSubItemCode));
		bus.setModuleItemKey(moduleItemKey);
		bus.setModuleSubItemKey(moduleSubItemKey);
		if(moduleItemKey != null) {
			bus.setQuestionnaireAnswerHeaderId( formDAO.getQuestionnairAnsHeaderId(moduleItemCode,
																		  moduleSubItemCode,
																		  moduleItemKey,
																		  moduleSubItemKey)		
											  );
			bus.setQuestionnaireCompleteFlag(formDAO.getQuestionnairCompleteFlag(bus.getQuestionnaireAnswerHeaderId()));
		}
		
		return bus;
	}
	
	private FormBuilderSectionsComponentDTO mapComponentEntityToDTO(FormBuilderSectionComponentEntity entity) {
		
		return FormBuilderSectionsComponentDTO.builder()
											  .componentId(entity.getFormBuilderSectCompId())
											  .sectionId(entity.getSectionId())											  
											  .componentType(entity.getComponentTypeCode())
											  .componentRefId(entity.getComponentRefId())											 
											  .componentData(entity.getComponentData())
											  .componentDescription(entity.getDescription())
											  .componentHeader(entity.getHeaderInstruction())
											  .componentFooter(entity.getFooterInstruction())
											  .build();
		
	}
	
	private FormBuilderSectionsDTO mapSectionEntityToDTO(FormBuilderSectionEntity formBuilderSectionEntity,
			Map<Integer, List<FormBuilderSectionsComponentDTO>> SectionComponentGrouping){
		
		FormBuilderSectionsDTO sectionDTO = new FormBuilderSectionsDTO();
		sectionDTO.setSectionId(formBuilderSectionEntity.getFormBuilderSectionId());
		sectionDTO.setSectionName(formBuilderSectionEntity.getSectionName());
		sectionDTO.setSectionOrder(formBuilderSectionEntity.getSectionOrderNumber());
		sectionDTO.setSectionDescription(formBuilderSectionEntity.getSectionName());
		sectionDTO.setSectionHelpText(formBuilderSectionEntity.getHelpText());
		sectionDTO.setSectionHeader(formBuilderSectionEntity.getHeaderInstruction());
		sectionDTO.setSectionFooter(formBuilderSectionEntity.getFooterInstruction());
		sectionDTO.setSectionComponent(
				SectionComponentGrouping.get(formBuilderSectionEntity.getFormBuilderSectionId()));
		
		return sectionDTO;
	}
	
	private Integer getPrimaryFormId(List<Integer> applicableFormId) {
		return (applicableFormId == null ? null : applicableFormId.get(0));
	}
	
	private FormBuilderSectionsComponentDTO getComponentInfoById(Integer componentId) {
		Optional<FormBuilderSectionComponentEntity> ComponentEntityOpt = componentRepository.findById(componentId);
		
		if(ComponentEntityOpt.isEmpty()) {			
			return new FormBuilderSectionsComponentDTO();
		}
		
		return mapComponentEntityToDTO(ComponentEntityOpt.get());
	}
	

	private FormComponentSaveResponse initialComponentSaveReponse(FormComponentSaveRequest request) {
		var response = new FormComponentSaveResponse();		
		response.setFormBuilderId(request.getFormBuilderId());
		response.setComponentId(request.getComponentId());		
		response.setModuleItemCode(request.getModuleItemCode());
		response.setModuleSubItemCode(request.getModuleSubItemCode());
		response.setModuleItemKey(request.getModuleItemKey());
		response.setModuleSubItemKey(request.getModuleSubItemKey());
		response.setDisabledSections(fetchDisabledSection(request.getFormBuilderId(),
														  request.getModuleItemCode(),
														  request.getModuleSubItemCode(),
														  request.getModuleItemKey(),
														  request.getModuleSubItemKey(),
														  request.getDocumentOwnerPersonId()));
		
		return response;
	}
	

	private FormBuilderProgElementEntity fetchProgrammedElementbyId(Integer id) {
		Optional<FormBuilderProgElementEntity> programmedElementOptional = programmedElementRepository.findById(id);
		if(programmedElementOptional.isEmpty()) {
			return null;
		}
		
		FormBuilderProgElementEntity programmedElementEntity = programmedElementOptional.get();
		return programmedElementEntity;
	}

	private List<Integer> fetchDisabledSection(Integer formBuilderId,
												String moduleItemCode,
												String moduleSubItemCode,
												String moduleItemKey,
												String moduleSubItemKey,
												String documentOwnerPersonId) {
		
		return formDAO.getDisabledSectionIds(formBuilderId,
											 moduleItemCode,
											 moduleItemKey,
											 documentOwnerPersonId);
	}
	
	private String getLoggedInUser() {		
		try {
			return AuthenticatedUser.getLoginUserName();
			
		}catch(Exception e) {
			return "nouser";
		}
	}

	public List<FormEvaluateValidationResponse> performFormValidation(FormValidationRequest formValidationRequest) {
		List<Integer> formBuilderIds = formValidationRequest.getFormBuilderIds();
		List<FormEvaluateValidationResponse> formEvaluateValidationResponseList = new ArrayList<>();
		formBuilderIds.forEach(formBuilderId -> {
			List<FormBuilderSectionsComponentDTO> components = formDAO.getComponentsForFormId(formBuilderId);
			Map<Integer, FormBuilderSectionsComponentDTO> componentMap = components.stream().collect(Collectors.toMap(FormBuilderSectionsComponentDTO::getComponentId, Function.identity()));
			Map<Integer, String> componentIdRuleIdMap = formDAO.getComponentIdRuleIdMap(formBuilderId, null, null);
			// Create a CompletableFuture for each entry in componentIdRuleIdMap
			List<CompletableFuture<List<FormEvaluateValidationResponse>>> futures = componentIdRuleIdMap.entrySet().stream()
			    .map(entry -> {
			        Integer componentId = entry.getKey();
			        String ruleIds = entry.getValue();
			        return CompletableFuture.supplyAsync(() -> formDAO.evaluateFormCompValidation(formValidationRequest, getLoggedInUser(), AuthenticatedUser.getLoginPersonId(), componentId, ruleIds));
			    })
			    .toList();
			// Combine all futures into a single CompletableFuture
			CompletableFuture<Void> allOfFutures = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
			// Wait for all futures to complete
			allOfFutures.join();
			// Retrieve the results from each future and add them to formEvaluateValidationResponseList
			futures.forEach(future -> formEvaluateValidationResponseList.addAll(future.join()));
			// Asynchronously execute formDAO.evaluateFormCompMandatoryValidation
			CompletableFuture<List<FormEvaluateValidationResponse>> mandatoryValidationFuture = CompletableFuture.supplyAsync(() ->
			        formDAO.evaluateFormCompMandatoryValidation(formBuilderId, null, null, formValidationRequest));
			// Wait for the mandatoryValidationFuture to complete and add its result to formEvaluateValidationResponseList
			formEvaluateValidationResponseList.addAll(mandatoryValidationFuture.join());
			formEvaluateValidationResponseList.parallelStream().forEach(response -> {
			    response.setFormBuilderId(formBuilderId);
			    FormBuilderSectionsComponentDTO component = componentMap.get(response.getComponentId());
			    response.setLabel(component.getLabel());
			    response.setSectionId(component.getSectionId());
			});
		});
		return formEvaluateValidationResponseList;
	}

	public List<FormEvaluateValidationResponse> performSectionValidation(FormValidationRequest formValidationRequest) {
		List<FormBuilderSectionComponentEntity> components = componentRepository.getAllComponentBySection(formValidationRequest.getFormBuilderSectionId());
		Map<Integer, FormBuilderSectionComponentEntity> componentMap = components.stream().collect(Collectors.toMap(FormBuilderSectionComponentEntity::getFormBuilderSectCompId, Function.identity()));
		List<FormEvaluateValidationResponse> processedResponses = new ArrayList<>();

		List<CompletableFuture<List<FormEvaluateValidationResponse>>> futures = new ArrayList<>();

		Map<Integer, String> componentIdRuleIdMap = formDAO.getComponentIdRuleIdMap(null, formValidationRequest.getFormBuilderSectionId(), null);

		for (Map.Entry<Integer, String> entry : componentIdRuleIdMap.entrySet()) {
		    Integer componentId = entry.getKey();
		    String ruleIds = entry.getValue();
		    
		    CompletableFuture<List<FormEvaluateValidationResponse>> future = CompletableFuture.supplyAsync(() ->
		            formDAO.evaluateFormCompValidation(formValidationRequest,
		                    getLoggedInUser(), AuthenticatedUser.getLoginPersonId(), componentId, ruleIds));
		    
		    futures.add(future);
		}

		CompletableFuture<Void> allOfFutures = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));

		CompletableFuture<List<FormEvaluateValidationResponse>> mandatoryValidationFuture = CompletableFuture.supplyAsync(() ->
		        formDAO.evaluateFormCompMandatoryValidation(null, formValidationRequest.getFormBuilderSectionId(), null, formValidationRequest));

		allOfFutures.thenCombine(mandatoryValidationFuture, (unused, mandatoryValidationResponses) -> {
		    processedResponses.addAll(mandatoryValidationResponses);

		    processedResponses.parallelStream().forEach(response -> {
		        FormBuilderSectionComponentEntity component = componentMap.get(response.getComponentId());
		        response.setLabel(component.getLabel());
		        response.setSectionId(component.getSectionId());
		    });

		    return processedResponses;
		}).join();

		return processedResponses;
	}

	public List<FormEvaluateValidationResponse> performComponentValidation(FormValidationRequest formValidationRequest) {
		List<FormEvaluateValidationResponse> formEvaluateValidationResponseList = new ArrayList<>();
		Map<Integer, String> componentIdRuleIdMap = formDAO.getComponentIdRuleIdMap(null, null, formValidationRequest.getFormBuilderSectCompId());
		List<CompletableFuture<List<FormEvaluateValidationResponse>>> futures = componentIdRuleIdMap.entrySet().stream()
			    .map(entry -> {
			        Integer componentId = entry.getKey();
			        String ruleIds = entry.getValue();
			        return CompletableFuture.supplyAsync(() -> formDAO.evaluateFormCompValidation(formValidationRequest, getLoggedInUser(), AuthenticatedUser.getLoginPersonId(), componentId, ruleIds));
			    })
			    .toList();
			// Combine all futures into a single CompletableFuture
			CompletableFuture<Void> allOfFutures = CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
			// Wait for all futures to complete
			allOfFutures.join();
			// Retrieve the results from each future and add them to formEvaluateValidationResponseList
			futures.forEach(future -> formEvaluateValidationResponseList.addAll(future.join()));
			// Asynchronously execute formDAO.evaluateFormCompMandatoryValidation
			CompletableFuture<List<FormEvaluateValidationResponse>> mandatoryValidationFuture = CompletableFuture.supplyAsync(() ->
			        formDAO.evaluateFormCompMandatoryValidation(null, null, formValidationRequest.getFormBuilderSectCompId(), formValidationRequest));
			// Wait for the mandatoryValidationFuture to complete and add its result to formEvaluateValidationResponseList
			formEvaluateValidationResponseList.addAll(mandatoryValidationFuture.join());
			return formEvaluateValidationResponseList;
	}

}
