package com.polus.formbuilder.service;

import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.appcorelib.customdataelement.controller.CustomDataElementController;
import com.polus.appcorelib.customdataelement.service.CustomDataElementService;
import com.polus.appcorelib.customdataelement.vo.CustomDataElementVO;
import com.polus.appcorelib.questionnaire.dto.QuestionnaireDataBus;
import com.polus.appcorelib.questionnaire.service.QuestionnaireService;
import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;
import com.polus.formbuilder.dto.FormBuilderSectionsDTO;
import com.polus.formbuilder.dto.FormResponseDTO;
import com.polus.formbuilder.entity.FormBuilderHeaderEntity;
import com.polus.formbuilder.model.BlankFormRequest;
import com.polus.formbuilder.model.BlankFormResponse;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementService;
import com.polus.formbuilder.repository.FormBuilderHeaderEntityRepository;

@Service
//This class will be revmoved
public class FormBuilderServiceImp  {

	@Autowired
	private ProgrammedElementService programmedElementService;

	@Autowired
	private QuestionnaireService questionnaireService;
	
	@Autowired
	private CustomDataElementService customDataElementService;

	@Autowired
	private FormBuilderHeaderEntityRepository fmHeaderRepository;

	public BlankFormResponse getFormForModule(BlankFormRequest request) {
		return null;
	}
	
	
	public BlankFormResponse getBlankFormByFormId(BlankFormRequest request) {
		// TODO Auto-generated method stub
		return null;
	}

	
	public BlankFormResponse GetBankFormbyModule(BlankFormRequest request) {

		Instant start = Instant.now();

		String performanceNote = "";

		List<FormBuilderHeaderEntity> ls = fmHeaderRepository.findAll();
		
		
		
		
		CustomDataElementVO customDataElementVO = new CustomDataElementVO();
		customDataElementVO.setCustomDataElementId(49);
		
		CustomDataElementController c ;

		customDataElementVO = customDataElementService.fetchCustomElementById(customDataElementVO);
		System.out.println(customDataElementVO);
		// QuestionnaireController
		QuestionnaireDataBus questionnaireDataBus = intialQuestionnaireDataBusObject(1055);

		start = Instant.now();

		List<Integer> qnrlsCF = Arrays.asList(1055, 1055, 1055, 1055, 1055, 1055, 1055, 1055, 1055, 1055);

		// Use CompletableFuture to parallelize the operations
		List<CompletableFuture<QuestionnaireDataBus>> futures = qnrlsCF.parallelStream()
				.map(i -> CompletableFuture.supplyAsync(
						() -> questionnaireService.getQuestionnaireDetails(intialQuestionnaireDataBusObject(i))))
				.collect(Collectors.toList());

		// Combine the completed futures into a list of QuestionnaireDataBus
		List<QuestionnaireDataBus> qnrDetailListCF = futures.parallelStream().map(CompletableFuture::join)
				.collect(Collectors.toList());

		performanceNote = performanceNote + "\n "
				+ " --------- Total Time(milliseconds) by COMPLETABLE FUTURE ---------------- : "
				+ Duration.between(start, Instant.now()).toMillis();


		ProgrammedElementModel programmedElementDTO = programmedElementService
				.getBlankResponse("OPA_COMP_UNCOMP");

		var primaryForm = FormResponseDTO.builder().formBuilderId(101).formBuilderNumber("10001")
				.formName("Testing Forms").formSections(

						Arrays.asList(FormBuilderSectionsDTO.builder().sectionId(201).sectionName("section Name")
								.sectionOrder(1).sectionDescription(performanceNote).sectionHeader("Section Header")
								.sectionFooter("Section Footer").sectionHelpText("HelpText")
								.sectionComponent(Arrays.asList(

										FormBuilderSectionsComponentDTO.builder().componentId(301).componentType("Q")
												.componentRefId("10001").ComponentOrder(1)
												.componentDescription("Component Questionnaire")
											//	.programmedElement(programmedElementDTO)
												.questionnaire(questionnaireDataBus)
												// .componentObject(new
												// GenericContainer<ProgrammedElement>(opaComUncomp))
												.build()

								)

								).build()

						)).build();

		var response = BlankFormResponse.builder().form(primaryForm)
				.applicableFormsBuilderIds(Arrays.asList(102, 103)).build();

		return response;
		
		/*
		 // Just keeping this code as reference, it will be removed before UAT
		var componentList = formUtil.getComponentsForFormId(primaryFormID);
		var sectionList = sectionRepository.getSectionDetailsFormId(primaryFormID);
		var formHeaderDetails = fmHeaderRepository.findById(primaryFormID).get();
		
		
		for(FormBuilderSectionsComponentDTO component : componentList) {
			
			if(component.getComponentType().equals(QUESTIONNAIR_COMPONENT)) {
				component.setQuestionnaire(questionnaireService
						.getQuestionnaireDetails(intialQuestionnaireDataBusObject(Integer.parseInt(component.getComponentRefId()))));
				
				
				
			}else if(component.getComponentType().equals(CUSTOM_ELEMENT_COMPONENT)) {
				component.setCustomElement(customDataElementService
						.fetchCustomElementById(intialCustomDataElementVO(Integer.parseInt(component.getComponentRefId()))));
				
				
			}else if(component.getComponentType().equals(PROGRAMMED_ELEMENT_COMPONENT)) {
				//Will program in next phase
			}
			
		}
		
		 Map<Integer, List<FormBuilderSectionsComponentDTO>> SectionComponentGrouping = componentList.stream()
		            .collect(Collectors.groupingBy(FormBuilderSectionsComponentDTO::getSectionId));
		
		List<FormBuilderSectionsDTO> sectionDTOList = new ArrayList<>();
		for(FormBuilderSectionEntity formBuilderSectionEntity :  sectionList) {
			FormBuilderSectionsDTO sectionDTO = new FormBuilderSectionsDTO();
			sectionDTO.setSectionId(formBuilderSectionEntity.getFormBuilderSectionId());
			sectionDTO.setSectionName(formBuilderSectionEntity.getSectionName());
			sectionDTO.setSectionOrder(formBuilderSectionEntity.getSectionOrderNumber());
			sectionDTO.setSectionDescription(formBuilderSectionEntity.getSectionName());
			sectionDTO.setSectionHelpText(formBuilderSectionEntity.getHelpText());
			sectionDTO.setSectionHeader(formBuilderSectionEntity.getHeaderInstruction());
			sectionDTO.setSectionFooter(formBuilderSectionEntity.getFooterInstruction());
			sectionDTO.setSectionComponent(SectionComponentGrouping.get(formBuilderSectionEntity.getFormBuilderSectionId()));
			sectionDTOList.add(sectionDTO);
		}
		
		FormResponseDTO formResponseDTO = new FormResponseDTO();
		formResponseDTO.setFormBuilderId(formHeaderDetails.getFormBuilderId());
		formResponseDTO.setFormBuilderNumber(formHeaderDetails.getFormBuilderNumber());
		formResponseDTO.setFormName(formHeaderDetails.getDescription());
		formResponseDTO.setFormSections(sectionDTOList);
		
		
		var response = BlankFormResponse.builder()
										.primaryForm(formResponseDTO)
										.applicableFormsBuilderIds(applicableFormId)
										.build();
		
		return response;
		 
		 */

	}


	private QuestionnaireDataBus intialQuestionnaireDataBusObject(Integer qnrId) {

		QuestionnaireDataBus bus = new QuestionnaireDataBus();
		bus.setQuestionnaireId(qnrId);
		return bus;
	}

	
}
