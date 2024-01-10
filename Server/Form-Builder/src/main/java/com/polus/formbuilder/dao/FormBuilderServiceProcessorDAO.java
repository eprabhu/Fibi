package com.polus.formbuilder.dao;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Component;

import com.polus.formbuilder.dto.FormBuilderSectionsComponentDTO;
import com.polus.formbuilder.entity.FormBuilderUsageEntity;

import jakarta.persistence.EntityManager;
import jakarta.persistence.ParameterMode;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.StoredProcedureQuery;
import jakarta.transaction.Transactional;
import jakarta.transaction.Transactional.TxType;

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
	
	public List<Integer> getDisabledSectionIds(Integer formBuilderId, String moduleCode, String subModuleCode,
			String documentOwnerPersonId) {
		List<Integer> disabledSectionId = new ArrayList<>();
		try {
				String sql = "select  t1.FORM_BUILDER_SECTION_ID "
					+ "from FORM_BUILDER_SECTION t1 "
					+ "where t1.FORM_BUILDER_ID  = :formBuilderId "
					+ "and t1.BUSINESS_RULE_ID is NOT NULL "
					+ "and t1.IS_ACTIVE = 'Y' "
					+ "and FN_EVALUATE_RULE(:moduleCode,:subModuleCode,NULL,t1.BUSINESS_RULE_ID,:documentOwnerPersonId,NULL,NULL) = 0 ";
					
	
			Query query = entityManager.createNativeQuery(sql);
			query.setParameter("formBuilderId", formBuilderId);
			query.setParameter("moduleCode", moduleCode);
			query.setParameter("subModuleCode", subModuleCode);
			query.setParameter("documentOwnerPersonId", documentOwnerPersonId);
			List<?> resultRows = query.getResultList();
			
			for (Object row : resultRows) {
				if (row instanceof Integer) {					
					disabledSectionId.add((Integer) row);
				}
			}
		}catch(Exception e) {
			e.printStackTrace();
		}

		return disabledSectionId;
	}

	@Transactional
	public List<FormBuilderUsageEntity> evaluateFormRule(List<FormBuilderUsageEntity> allApplicableForms, Integer moduleItemKey,
			String moduleItemCode, String moduleSubItemCode, String logginPersonId, String updateUser, String moduleSubItemKey) {
		List<FormBuilderUsageEntity> applicableForm = new ArrayList<>();
		if (allApplicableForms != null && !allApplicableForms.isEmpty()) {
			for (FormBuilderUsageEntity formUsage : allApplicableForms) {
				Integer ruleId = (formUsage.getBusinessRuleId() == null ? 0 : formUsage.getBusinessRuleId());
				if (ruleId == 0) {
					applicableForm.add(formUsage);
				} else {
					boolean isrulePassed = evaluateRule(moduleItemCode, moduleSubItemCode, moduleItemKey, ruleId, logginPersonId, updateUser, moduleSubItemKey);
					if (isrulePassed) {
						applicableForm.add(formUsage);
					}
				}
			}
		}		
		return applicableForm;
	}

	@Transactional(value = TxType.SUPPORTS)
	private Boolean evaluateRule(String moduleItemCode, String moduleSubItemCode, Integer moduleItemKey, Integer ruleId,
			String logginPersonId, String updateUser, String moduleSubItemKey) {
		try {
			StoredProcedureQuery query = entityManager.createStoredProcedureQuery("rule_evaluation")
					.registerStoredProcedureParameter(1, String.class, ParameterMode.IN)
					.registerStoredProcedureParameter(2, String.class, ParameterMode.IN)
					.registerStoredProcedureParameter(3, String.class, ParameterMode.IN)
					.registerStoredProcedureParameter(4, Integer.class, ParameterMode.IN)
					.registerStoredProcedureParameter(5, Integer.class, ParameterMode.IN)
					.registerStoredProcedureParameter(6, String.class, ParameterMode.IN)
					.registerStoredProcedureParameter(7, String.class, ParameterMode.IN)
					.registerStoredProcedureParameter(8, String.class, ParameterMode.IN);
			query.setParameter(1, moduleItemCode);
			query.setParameter(2, moduleItemCode);
			query.setParameter(3, moduleSubItemCode);
			query.setParameter(4, moduleItemKey);
			query.setParameter(5, ruleId);
			query.setParameter(6, logginPersonId);
			query.setParameter(7, updateUser);
			query.setParameter(8, moduleSubItemKey);
			query.execute();
			Long result = (Long) query.getSingleResult();
			return (result != null && result.equals(1L)) ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
}
