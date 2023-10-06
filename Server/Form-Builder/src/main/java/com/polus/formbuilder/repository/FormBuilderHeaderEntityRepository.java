package com.polus.formbuilder.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderHeaderEntity;

@Repository
public interface FormBuilderHeaderEntityRepository extends  JpaRepository<FormBuilderHeaderEntity, Integer>  {

	@Query(value = "SELECT * FROM FORM_BUILDER_HEADER WHERE FORM_BUILDER_NUMBER = ?1", nativeQuery = true)
	FormBuilderHeaderEntity getFormbyFormNumber(String formNumber);
	
	
//    @Query("SELECT NEW com.polus.formbuilder.dto.FormResponseDTO(f.formBuilderId, f.formBuilderNumber,"
//		  + " f.description, s.formBuilderSectionId, s.sectionName, s.sectionOrderNumber, s.description,"
//		  + " s.businessRuleId, s.helpText, s.headerInstruction, s.footerInstruction, c.formBuilderSectCompId,"
//		  + " c.description, c.componentTypeCode, c.componentOrderNumber, c.componentRefId, c.componentData,"
//		  + " c.headerInstruction, c.footerInstruction, c.isActive) " +
//		    "FROM FormBuilderHeaderEntity f " +
//            "JOIN f.sections s " +
//            "JOIN s.sectionComponents c " +
//            "WHERE f.formBuilderId = :formBuilderId AND c.isActive = 'Y' AND s.isActive = 'Y'")
//     FormResponseDTO getFormDetails(@Param("formBuilderId") Integer formBuilderId);

}
