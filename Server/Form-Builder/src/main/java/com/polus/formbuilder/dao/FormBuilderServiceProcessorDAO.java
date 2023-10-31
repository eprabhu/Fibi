package com.polus.formbuilder.dao;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Component;

import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;

@Component
public class FormBuilderServiceProcessorDAO {

	@PersistenceContext
	private EntityManager entityManager;

	public List<Integer> getApplicableFormIds(String moduleCode, String subModuleCode,
			String documentOwnerPersonId) {
		
		String sql = """
				  	select t.FORM_BUILDER_ID,t.FORM_ORDER_NUMBER
				from
				(
				  		SELECT t1.FORM_BUILDER_ID,t1.FORM_ORDER_NUMBER
					FROM form_builder_usage t1
					where t1.MODULE_CODE = :moduleCode
					and t1.SUB_MODULE_CODE = :subModuleCode
					and t1.BUSINESS_RULE_ID is NULL
					and t1.IS_ACTIVE = 'Y'

					UNION

					SELECT t1.FORM_BUILDER_ID,t1.FORM_ORDER_NUMBER
					FROM form_builder_usage t1
					where t1.MODULE_CODE = :moduleCode
					and t1.SUB_MODULE_CODE = :subModuleCode
					and t1.BUSINESS_RULE_ID is NOT NULL
					and t1.IS_ACTIVE = 'Y'
					and FN_EVALUATE_RULE(:moduleCode,:subModuleCode,NULL,t1.BUSINESS_RULE_ID,:documentOwnerPersonId,NULL,NULL) = 1

					) t ORDER by t.FORM_ORDER_NUMBER

							 """;

		Query query = entityManager.createNativeQuery(sql);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("subModuleCode", subModuleCode);
		query.setParameter("documentOwnerPersonId", documentOwnerPersonId);
		List<?> resultRows = query.getResultList();

		List<Integer> formList = new ArrayList<>();
		for (Object row : resultRows) {
			if (row instanceof Object[]) {
				Object[] rowData = (Object[]) row;
				formList.add((Integer) rowData[0]);
			}
		}

		return formList;
	}

	
	public List<FormBuilderSectionsComponentDTO> getComponentsForFormId(Integer formId) {

		String sql = """

					SELECT
					t1.FORM_BUILDER_SECT_COMP_ID,
					t1.FORM_BUILDER_SECTION_ID,
					t1.COMPONENT_TYPE_CODE,
					t1.COMPONENT_REF_ID,
					t1.COMPONENT_ORDER_NUMBER,
					t1.COMPONENT_DATA,
					t1.HEADER_INSTRUCTION,
					t1.FOOTER_INSTRUCTION
					FROM form_builder_section_component t1
					inner join form_builder_section t2 on t1.FORM_BUILDER_SECTION_ID = t2.FORM_BUILDER_SECTION_ID
					inner join form_builder_header t3 on t2.FORM_BUILDER_ID = t3.FORM_BUILDER_ID
					where t1.IS_ACTIVE = 'Y'
					and t2.IS_ACTIVE = 'Y'
					and t3.FORM_BUILDER_ID = :formId
					ORDER BY t1.COMPONENT_ORDER_NUMBER


				""";
		Query query = entityManager.createNativeQuery(sql);
		query.setParameter("formId", formId);
		List<?> resultRows = query.getResultList();

		List<FormBuilderSectionsComponentDTO> componentList = new ArrayList<>();
		for (Object row : resultRows) {
			if (row instanceof Object[]) {
				Object[] rowData = (Object[]) row;
				var componentDto = FormBuilderSectionsComponentDTO.builder()
									.componentId((Integer) rowData[0])
									.sectionId((Integer) rowData[1])
									.componentType((String) rowData[2])
									.componentRefId((String) rowData[3])
									.ComponentOrder((Integer) rowData[4])
									.componentData((String) rowData[5])
									.componentHeader((String) rowData[6])
									.componentFooter((String) rowData[7])
									.build();
				
				componentList.add(componentDto);
			}
		}

		return componentList;
	}
	
	public Integer getQuestionnairAnsHeaderId(String moduleItemCode,
											  String moduleSubItemCode,
											  String moduleItemKey,
											  String moduleSubItemKey
											 ) {
		
		String sql = "SELECT t1.QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header t1 " +
		             "WHERE t1.MODULE_ITEM_CODE = :moduleItemCode " +
		             "AND t1.MODULE_SUB_ITEM_CODE = :moduleSubItemCode " +
		             "AND t1.MODULE_ITEM_KEY = :moduleItemKey " +
		             "AND t1.MODULE_SUB_ITEM_KEY = :moduleSubItemKey";

		Query query = entityManager.createNativeQuery(sql);
		query.setParameter("moduleItemCode", moduleItemCode);
		query.setParameter("moduleSubItemCode", moduleSubItemCode);
		query.setParameter("moduleItemKey", moduleItemKey);
		query.setParameter("moduleSubItemKey", moduleSubItemKey);
		
		List<?> resultRows = query.getResultList();
		
		return (Integer) getSingleResultOrNull(resultRows);
	}
	
	
	public static <T> T getSingleResultOrNull(List<T> resultRows) {
	    if (resultRows != null && !resultRows.isEmpty()) {
	        return resultRows.get(0);
	    }
	    return null;
	}
	
}
