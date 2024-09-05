package com.polus.integration.entity.enrich.dao;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.entity.enrich.dto.EntityEnrichAPIResponse;
import com.polus.integration.entity.enrich.dto.EntityEnrichAPIResponse.Organization;
import com.polus.integration.constant.Constant;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.DefaultType;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.DetailedAddress;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.DunsControlStatus;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.IndustryCode;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.RegistrationNumber;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.Telephone;

@Transactional
@Repository
public class EntityEnrichDAO {

    @Autowired
    private JdbcTemplate jdbcTemplate;
    
        
   public void refreshEntityHeaderInfo(Integer entityId, String actionPersonId, Organization res) {
	   String sql = "{call ENRICH_ENTITY_HEADER(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";

       jdbcTemplate.update(sql,
    		   entityId, 
    		   res.getPrimaryName(),
    		   res.getMultilingualPrimaryName(),
    		   res.getPriorName(),
    		   res.getTradeStyleNames(),
    		   res.getDuns(),
    		   res.getUei(),
    		   res.getCageNumber(),
    		   getOwnershipType(res.isPubliclyTradedCompany()),
    		   getOperatingStatusType(res.getDunsControlStatus()),
    		   getBusinessEntityType(res.getBusinessEntityType()),
    		   res.getWebsiteAddress(),
    		   res.getStartDate(), 
    		   res.getIncorporatedDate(),
    		   res.getNumberOfEmployees(),
    		   res.getCertifiedEmail(),
    		   res.getSummary(),
    		   res.getDefaultCurrency(),
    		   getPrimaryTelephone(res.getTelephone()),
    		   getLine1(res.getPrimaryAddress()),
    		   getLine2(res.getPrimaryAddress()),
    		   getCity(res.getPrimaryAddress()),
    		   getState(res.getPrimaryAddress()),
    		   getPostCode(res.getPrimaryAddress()),
    		   getCountry(res.getPrimaryAddress()),
    		   res.getHumanSubAssurance(),
    		   res.getAnimalWelfareAssurance(),
    		   res.getAnimalAccreditaion(),
    		   actionPersonId
    		   );
              
   }
   
	public void refreshEntityIndustryCode(Integer entityId, String actionPersonId, List<IndustryCode> industryCodes) {

		if (industryCodes == null || industryCodes.isEmpty()) {
			return;
		}
		String sql = "{call ENRICH_ENTITY_INDUSTRY_CATE(?,?,?,?,?,?)}";
		for (IndustryCode input : industryCodes) {

			jdbcTemplate.update(sql, 
					entityId,
					actionPersonId,
					input.getCode(),
					input.getDescription(),
					input.getTypeDescription(), 
					input.getTypeDnBCode());
		}

	}
   
   public void refreshEntityRegistration(Integer entityId, String actionPersonId, List<RegistrationNumber> registrationNumbers) {
	   
	   			if (registrationNumbers == null || registrationNumbers.isEmpty()) {
					return;
				}
				String sql = "{call ENRICH_ENTITY_REGISTRATION(?,?,?,?,?)}";
				for (RegistrationNumber input : registrationNumbers) {

					jdbcTemplate.update(sql, 
							entityId,							
							input.getRegistrationNumber(),
							input.getTypeDescription(),							
							input.getTypeDnBCode(),
							actionPersonId);
				}	   
	   
   }

   public void refreshEntityTelephone(Integer entityId, String actionPersonId, List<Telephone> telephone) {
   
	   		if (telephone == null || telephone.isEmpty()) {
				return;
			}
			String sql = "{call ENRICH_ENTITY_TELEPHONE(?,?,?,?)}";
			for (Telephone input : telephone) {

				jdbcTemplate.update(sql, 
						entityId,							
						input.getTelephoneNumber(),
						input.getIsdCode(),
						actionPersonId);
			}	  
   }
   
   public void refreshEntityTradeName(Integer entityId, String actionPersonId, EntityEnrichAPIResponse response) {
   
   }
   
   private String getLine1(DetailedAddress address) {
	   if(address != null && address.getStreetAddress() != null) {
		   return address.getStreetAddress().getLine1();
	   }
	   return null;
   }
   
   private String getLine2(DetailedAddress address) {
	   if(address != null && address.getStreetAddress() != null) {
		   return address.getStreetAddress().getLine2();
	   }
	   return null;
   }
   
   private String getCity(DetailedAddress address) {
	   if(address != null && address.getAddressLocality() != null) {
		   return address.getAddressLocality().getName();
	   }
	   return null;
   }
   
   private String getState(DetailedAddress address) {
	   if(address != null && address.getAddressRegion() != null) {
		   return address.getAddressRegion().getAbbreviatedName();
	   }
	   return null;
   }
   
   
   private String getCountry(DetailedAddress address) {
	   if(address != null && address.getAddressCountry() != null) {
		   return address.getAddressCountry().getIsoAlpha2Code();
	   }
	   return null;
   }
   private String getPostCode(DetailedAddress address) {
	   if(address != null && address.getPostalCode() != null) {
		   return address.getPostalCode();
	   }
	   return null;
   }   
   private String getPrimaryTelephone(List<Telephone> telephone) {
	   if(telephone == null) {
		   return null;
	   }
	   
	   if(telephone != null && telephone.isEmpty()) {
		   return null;
	   }
	   
	   if(telephone != null && telephone.get(0) != null) {
		   return telephone.get(0).getTelephoneNumber();
	   }	   
	   return null;
   }
   
   
   private String getOwnershipType(boolean isPubliclyTradedCompany) {
	   if(isPubliclyTradedCompany) {
		   return "1" ; //Publicly Traded Company		   
	   }	   
	   return "2" ; //Privately owned	
   }
   
   private Integer getOperatingStatusType(DunsControlStatus dunsControlStatus) {
	   if(dunsControlStatus != null && dunsControlStatus.getOperatingStatus()!= null) {
		   return dunsControlStatus.getOperatingStatus().getDnbCode();
	   }
	   return null;
   }
   private Integer getBusinessEntityType(DefaultType businessEntityType) {
	   if(businessEntityType != null ) {
		   
		   if(businessEntityType.getDnbCode()== 0) {
			   return null;
		   }
		   
		   return businessEntityType.getDnbCode();
	   }
	   return null;
   }  
}
