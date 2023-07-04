package com.polus.appcoigraph.dao;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.neo4j.core.Neo4jClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import com.polus.appcoigraph.entity.Award;
import com.polus.appcoigraph.entity.COIEntity;
import com.polus.appcoigraph.entity.Country;
import com.polus.appcoigraph.entity.Disclosure;
import com.polus.appcoigraph.entity.Person;
import com.polus.appcoigraph.entity.Proposal;
import com.polus.appcoigraph.entity.Sponsor;
import com.polus.appcoigraph.entity.TravelDisclosure;
import com.polus.appcoigraph.entity.Unit;
import com.polus.appcoigraph.repository.AwardRepository;
import com.polus.appcoigraph.repository.COIDisclosureRepository;
import com.polus.appcoigraph.repository.CountryRepository;
import com.polus.appcoigraph.repository.EntityRepository;
import com.polus.appcoigraph.repository.PersonRepository;
import com.polus.appcoigraph.repository.ProposalRepository;
import com.polus.appcoigraph.repository.SponsorRepository;
import com.polus.appcoigraph.repository.TravelDisclosureRepository;
import com.polus.appcoigraph.repository.UnitRepository;

@Repository
public class COIImportGraphDataDao {

	@Autowired
	private EntityRepository entityRepository;
	
	@Autowired
	private CountryRepository countryRepository;

	@Autowired
	private PersonRepository personRepository;
	
	@Autowired
	private ProposalRepository proposalRepository;
	
	@Autowired
	private AwardRepository awardRepository;
		
	@Autowired
	private SponsorRepository sponsorRepository;
	
	@Autowired
	private UnitRepository unitRepository;
	
	@Autowired
	private TravelDisclosureRepository travelRepository;
	
	@Autowired
	private COIDisclosureRepository coiRepository;
	
	@Autowired
	private Neo4jClient neo4jClient;

	@Autowired
	private JdbcTemplate jdbcTemplate;


	@Value("${spring.data.neo4j.database}")
	private String schema;
	
		
	public void importDataFromRDBMS() {
		importCountry();
		importEntity();
		importPerson();
		importProposal();
		importAward();
		importSponsor();
		importUnit();
		importCOIDisclosure();
		importTravelDisclosure();
		linkEntityCountry();
		linkPersonCountry();
		linkPersonDependentEntityRelationship();
		linkPersonSpouseEntityRelationship();
		linkPersonSelfEntityRelationship();
		linkProposalEntityRelationship();
		linkProposalSponsor();
		linkProposalPrimeSponsor();
		linkProposalUnit();
		linkEntityToEntityRelationship();
		linkAwardEntityRelationship();
		linkAwardPrimeSponsor();
		linkAwardSponsor();
		linkAwardUnit();
		linkPersonCOIDisclosure();
		linkEntityCOIDisclosure();
		linkCOIDisclosureAward();
		linkCOIDisclosureProposal();
		linkPersonTravelDisclosure();
		linkEntityTravelDisclosure();	
		
	}
	
	
	
	public void importCountry() {
		 String query = "SELECT CONCAT('CON',COUNTRY_CODE) AS ID, COUNTRY_NAME,COUNTRY_CODE,CURRENCY_CODE from country ";
	        List<Country> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	Country country = new Country();
	        	country.setId(resultSet.getString("ID"));
	        	country.setCountryCode(resultSet.getString("COUNTRY_CODE"));
	        	country.setCountryName(resultSet.getString("COUNTRY_NAME"));
	        	country.setCurrency(resultSet.getString("CURRENCY_CODE"));	        	
	            return country;
	        });
	        countryRepository.saveAll(ls);
   }
	
	public void importEntity() {
		 String query = "SELECT \r\n"
		 		+ "CONCAT('ENT',t1.ENTITY_ID) as ID,\r\n"
		 		+ "t1.ENTITY_NUMBER,\r\n"
		 		+ "t1.ENTITY_NAME as NAME,\r\n"
		 		+ "t2.DESCRIPTION as STATUS,\r\n"
		 		+ "t3.DESCRIPTION as TYPE,\r\n"
		 		+ "t1.COUNTRY_CODE,\r\n"
		 		+ "t4.COUNTRY_NAME,\r\n"
		 		+ "t1.WEB_URL\r\n"
		 		+ "FROM entity t1\r\n"
		 		+ "inner join entity_status t2 on t1.entity_status_code = t2.entity_status_code\r\n"
		 		+ "inner join entity_type t3 on t1.entity_type_code = t3.entity_type_code\r\n"
		 		+ "left outer join country t4 on t1.COUNTRY_CODE = t4.COUNTRY_CODE\r\n"
		 		+ "WHERE t1.VERSION_STATUS = 'ACTIVE'";
	        List<COIEntity> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	COIEntity entity = new COIEntity();
	        	entity.setId(resultSet.getString("ID"));
	        	entity.setEntityNumber(resultSet.getString("ENTITY_NUMBER"));
	        	entity.setEntityName(resultSet.getString("NAME"));
	        	entity.setStatus(resultSet.getString("STATUS"));
	        	entity.setType(resultSet.getString("TYPE"));
	        	entity.setCountryName(resultSet.getString("COUNTRY_NAME"));
	        	entity.setCountryCode(resultSet.getString("COUNTRY_CODE"));
	        	
//	        	Optional<Country> optCountry = countryRepository.findById("CON"+resultSet.getString("COUNTRY_CODE"));
//	        	if(optCountry.isPresent()) {
//	        		entity.setCountryOwned(optCountry.get());
//	        	}	        	
	        	
	        	return entity;
	        });
	        entityRepository.saveAll(ls);
  }
	
	public void importPerson() {
		 String query = "select CONCAT('PER',PERSON_ID) AS ID, PERSON_ID,FULL_NAME,UNIT_NAME as HOME_UNIT,t1.COUNTRY_OF_CITIZENSHIP as COUNTRY_CODE, t4.COUNTRY_NAME,\r\n"
		 		+ "CASE   WHEN STATUS = 'A' THEN 'Active' ELSE 'Inactive' END AS STATUS \r\n"
		 		+ "from person t1\r\n"
		 		+ "left outer join unit t2 on t1.home_unit = t2.unit_number\r\n"
		 		+ "left outer join country t4 on t1.COUNTRY_OF_CITIZENSHIP = t4.COUNTRY_CODE\r\n"
		 		+ "where t1.COUNTRY_OF_CITIZENSHIP is not null ";
		
	        	List<Person> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	Person person = Person.builder()
	        						  .id(resultSet.getString("ID"))
	        						  .personId(resultSet.getString("PERSON_ID"))
	        						  .fullName(resultSet.getString("FULL_NAME"))
	        						  .homeUnit(resultSet.getString("HOME_UNIT"))
	        						  .countryCode(resultSet.getString("COUNTRY_CODE"))
	        						  .countryName(resultSet.getString("COUNTRY_NAME"))
	        						  .status(resultSet.getString("STATUS"))
	        						  .build();
	        	      	
	            return person;
	        });
	        personRepository.saveAll(ls);
  }
	
	public void importProposal() {
		 String query = "SELECT distinct CONCAT('EPS',t1.PROPOSAL_ID) AS ID, t1.PROPOSAL_ID, t1.TITLE, DATE_FORMAT(t1.START_DATE, '%m-%d-%Y') as START_DATE ,\r\n"
		 		+ "DATE_FORMAT(t1.END_DATE, '%m-%d-%Y') as END_DATE,\r\n"
		 		+ "t2.SPONSOR_CODE,\r\n"
		 		+ "t1.SPONSOR_NAME,\r\n"
		 		+ "t2.PRIME_SPONSOR_CODE,\r\n"
		 		+ "t3.SPONSOR_NAME as PRIME_SPONSOR_NAME,\r\n"
		 		+ "t1.LEAD_UNIT_NUMBER as UNIT_NUMBER,\r\n"
		 		+ "t1.LEAD_UNIT_NAME, t1.TYPE_OF_FUNDING_AGENCY, t1.PI_NAME\r\n"
		 		+ "FROM eps_proposal_v t1\r\n"
		 		+ "inner join eps_proposal t2 on t1.PROPOSAL_ID = t2.PROPOSAL_ID\r\n"
		 		+ "left outer join sponsor t3 on t2.PRIME_SPONSOR_CODE = t3.SPONSOR_CODE;\r\n"
		 		+ "";
		 		List<Proposal> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
  	
				Proposal proposal = Proposal.builder()
											.id(resultSet.getString("ID"))
											.proposalId(resultSet.getString("PROPOSAL_ID"))
											.title(resultSet.getString("TITLE"))
											.startDate(resultSet.getString("START_DATE"))
											.endDate(resultSet.getString("END_DATE"))
											.sponsorCode(resultSet.getString("SPONSOR_CODE"))
											.sponsorName(resultSet.getString("SPONSOR_NAME"))
											.primeSponsorCode(resultSet.getString("PRIME_SPONSOR_CODE"))
											.primeSponsorName(resultSet.getString("PRIME_SPONSOR_NAME"))
											.unitNumber(resultSet.getString("UNIT_NUMBER"))
											.leadUnitName(resultSet.getString("LEAD_UNIT_NAME"))
											.typeOfFunding(resultSet.getString("TYPE_OF_FUNDING_AGENCY"))
											.piName(resultSet.getString("PI_NAME"))
											.build();
	            return proposal;
	        });
	        proposalRepository.saveAll(ls);
  }	

	public void importAward() {
		 String query = "SELECT \r\n"
		 		+ "distinct CONCAT('AWD',AWARD_NUMBER) AS ID,AWARD_NUMBER,TITLE,AWARD_TYPE,LEAD_UNIT_NUMBER as UNIT_NUMBER,LEAD_UNIT,SPONSOR_CODE,SPONSOR_NAME,PRIME_SPONSOR_CODE,PRIME_SPONSOR_NAME,\r\n"
		 		+ "DATE_FORMAT(BEGIN_DATE, '%m-%d-%Y') as START_DATE,\r\n"
		 		+ "DATE_FORMAT(FINAL_EXPIRATION_DATE, '%m-%d-%Y') as END_DATE,\r\n"
		 		+ "PI_NAME\r\n"
		 		+ "FROM award_v";
		 		List<Award> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {

		 			Award award = Award.builder()
											.id(resultSet.getString("ID"))
											.awardNumber(resultSet.getString("AWARD_NUMBER"))
											.title(resultSet.getString("TITLE"))
											.startDate(resultSet.getString("START_DATE"))
											.endDate(resultSet.getString("END_DATE"))
											.sponsorCode(resultSet.getString("SPONSOR_CODE"))
											.sponsorName(resultSet.getString("SPONSOR_NAME"))
											.primeSponsorCode(resultSet.getString("PRIME_SPONSOR_CODE"))
											.primeSponsorName(resultSet.getString("PRIME_SPONSOR_NAME"))
											.unitNumber(resultSet.getString("UNIT_NUMBER"))
											.leadUnitName(resultSet.getString("LEAD_UNIT"))
											.awardType(resultSet.getString("AWARD_TYPE"))
											.piName(resultSet.getString("PI_NAME"))
											.build();
	            return award;
	        });
	        awardRepository.saveAll(ls);
 }
	public void importSponsor() {
		 String query = "SELECT CONCAT('SPN',SPONSOR_CODE) AS ID, SPONSOR_CODE,SPONSOR_NAME\r\n"
		 		+ "				FROM sponsor ";
	        List<Sponsor> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	Sponsor sponsor = Sponsor.builder()
	        							 .id(resultSet.getString("ID"))
	        							 .sponsorCode(resultSet.getString("SPONSOR_CODE"))
	        							 .sponsorName(resultSet.getString("SPONSOR_NAME"))
	        							 .build();
	            return sponsor;
	        });
	        sponsorRepository.saveAll(ls);
  }
	public void importUnit() {
		 String query = "SELECT CONCAT('UNT',UNIT_NUMBER) AS ID,UNIT_NUMBER, UNIT_NAME FROM unit ";
	        List<Unit> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	Unit unit = Unit.builder()
						 .id(resultSet.getString("ID"))
						 .unitNumber(resultSet.getString("UNIT_NUMBER"))
						 .unitName(resultSet.getString("UNIT_NAME"))
						 .build();        	
	            return unit;
	        });
	        unitRepository.saveAll(ls);
  }
	public void linkEntityCountry() {
       String cypherQuery = "MATCH (a:Entity), (b:Country) WHERE a.country_code = b.country_code MERGE (a)-[r:BELONGS_TO]->(b)  ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }
	public void linkPersonCountry() {
       String cypherQuery = "MATCH (a:Person), (b:Country) WHERE a.country_code = b.country_code MERGE (a)-[r:CITIZEN_OF]->(b)  ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }	
	
	public void linkPersonSelfEntityRelationship() {

		String query = "SELECT distinct t1.PERSON_ID, t1.INVOLVEMENT_START_DATE,t1.INVOLVEMENT_END_DATE,t1.ENTITY_ID as ENTITY_ID,\r\n"
				+ "		t1.ENTITY_NUMBER\r\n"
				+ "		FROM person_entity t1\r\n"
				+ "		inner join person_entity_relationship t2 on t1.PERSON_ENTITY_ID  = t2.PERSON_ENTITY_ID\r\n"
				+ "		where t2.VALID_PERSON_ENTITY_REL_TYPE_CODE = 1";
	
		jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (person:Person {person_id: '" + resultSet.getString("PERSON_ID")+"' })\r\n"
						+ "				MATCH (entity:Entity {entity_number: '" +resultSet.getString("ENTITY_NUMBER")+"' })\r\n"
						+ "				MERGE (person)-[x:SELF_RELATIONSHIP]->(entity)\r\n"
						+ "				SET x.involvement_start_date = '"+resultSet.getString("INVOLVEMENT_START_DATE") +"' ,\r\n"
						+ "					x.involvement_end_date = '"+ resultSet.getString("INVOLVEMENT_END_DATE") +"'	  ";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });
		 
   }
	
	public void linkPersonSpouseEntityRelationship() {

		String query = "SELECT t1.PERSON_ID, t1.INVOLVEMENT_START_DATE,t1.INVOLVEMENT_END_DATE,t1.ENTITY_ID as ENTITY_ID,\r\n"
				+ "		t1.ENTITY_NUMBER\r\n"
				+ "		FROM person_entity t1\r\n"
				+ "		inner join person_entity_relationship t2 on t1.PERSON_ENTITY_ID  = t2.PERSON_ENTITY_ID\r\n"
				+ "		where t2.VALID_PERSON_ENTITY_REL_TYPE_CODE = 2";
	
		jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (person:Person {person_id: '" + resultSet.getString("PERSON_ID")+"' })\r\n"
						+ "				MATCH (entity:Entity {entity_number: '" +resultSet.getString("ENTITY_NUMBER")+"' })\r\n"
						+ "				MERGE (person)-[x:SPOUSE_RELATIONSHIP]->(entity)\r\n"
						+ "				SET x.involvement_start_date = '"+resultSet.getString("INVOLVEMENT_START_DATE") +"' ,\r\n"
						+ "					x.involvement_end_date = '"+ resultSet.getString("INVOLVEMENT_END_DATE") +"'  ";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });
		 
   }	

	public void linkPersonDependentEntityRelationship() {

		String query = "SELECT t1.PERSON_ID, t1.INVOLVEMENT_START_DATE,t1.INVOLVEMENT_END_DATE,t1.ENTITY_ID as ENTITY_ID,\r\n"
				+ "		t1.ENTITY_NUMBER\r\n"
				+ "		FROM person_entity t1\r\n"
				+ "		inner join person_entity_relationship t2 on t1.PERSON_ENTITY_ID  = t2.PERSON_ENTITY_ID\r\n"
				+ "		where t2.VALID_PERSON_ENTITY_REL_TYPE_CODE = 3";
	
		jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (person:Person {person_id: '" + resultSet.getString("PERSON_ID")+"' })\r\n"
						+ "				MATCH (entity:Entity {entity_number: '" +resultSet.getString("ENTITY_NUMBER")+"' })\r\n"
						+ "				MERGE (person)-[x:DEPENDANT_RELATIONSHIP]->(entity)\r\n"
						+ "				SET x.involvement_start_date = '"+resultSet.getString("INVOLVEMENT_START_DATE") +"' ,\r\n"
						+ "					x.involvement_end_date = '"+ resultSet.getString("INVOLVEMENT_END_DATE") +"'	  ";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });
		 
   }	

	
	public void linkProposalEntityRelationship() {

		String query = "select distinct	 \r\n"
				+ "t3.ENTITY_NUMBER as ENTITY_NUMBER,\r\n"
				+ "t2.MODULE_ITEM_KEY AS PROPOSAL_ID              \r\n"
				+ "from coi_disclosure t1\r\n"
				+ "inner join coi_discl_ent_proj_details t2 on t1.DISCLOSURE_ID = t1.DISCLOSURE_ID\r\n"
				+ "inner join person_entity t3 on t2.PERSON_ENTITY_ID = t3.PERSON_ENTITY_ID			   \r\n"
				+ "where t2.MODULE_CODE = 3";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
	
				String cypherQuery = "	MATCH (proposal:Proposal {proposal_id: '"+ resultSet.getString("PROPOSAL_ID") +"' })\r\n"
						+ "MATCH (entity:Entity {entity_number: '"+ resultSet.getString("ENTITY_NUMBER") +"' })\r\n"
						+ "MERGE (entity)-[x:ENTITY_PROPOSAL_RELATIONSHIP]->(proposal)				\r\n"
						+ " ";				
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });
		 
   }		

	
	public void linkAwardEntityRelationship() {

		String query = "\r\n"
				+ "select distinct\r\n"
				+ "t3.ENTITY_NUMBER as ENTITY_NUMBER,\r\n"
				+ "t4.AWARD_NUMBER\r\n"
				+ "from coi_disclosure t1\r\n"
				+ "inner join coi_discl_ent_proj_details t2 on t1.DISCLOSURE_ID = t1.DISCLOSURE_ID\r\n"
				+ "inner join person_entity t3 on t2.PERSON_ENTITY_ID = t3.PERSON_ENTITY_ID	\r\n"
				+ "inner join award t4 on t4.award_id = t2.MODULE_ITEM_KEY\r\n"
				+ "where t2.MODULE_CODE = 1 \r\n"
				+ "";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
	
				String cypherQuery = "	MATCH (award:Award {award_number: '"+ resultSet.getString("AWARD_NUMBER") +"' })\r\n"
						+ "MATCH (entity:Entity {entity_number: '"+ resultSet.getString("ENTITY_NUMBER") +"' })\r\n"
						+ "MERGE (entity)-[x:ENTITY_AWARD_RELATIONSHIP]->(award)	\r\n"
						+ " ";				
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });		 
   }		
		
	public void linkAwardSponsor() {
       String cypherQuery = "MATCH (a:Award), (b:Sponsor) WHERE a.sponsor_code = b.sponsor_code MERGE (a)-[r:AWARD_SPONSORED_BY]->(b)  ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }
	
	public void linkAwardPrimeSponsor() {
       String cypherQuery = "MATCH (a:Award), (b:Sponsor) WHERE a.prime_sponsor_code = b.sponsor_code MERGE (a)-[r:AWARD_PRIME_SPONSORED_BY]->(b)  ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }	
	
	public void linkAwardUnit() {
       String cypherQuery = "MATCH (a:Award), (b:Unit) WHERE a.unit_number = b.unit_number MERGE (a)-[r:AWARD_OWNED_BY]->(b)";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }

	public void linkProposalSponsor() {
       String cypherQuery = " MATCH (a:Proposal), (b:Sponsor) WHERE a.sponsor_code = b.sponsor_code MERGE (a)-[r:PROPOSAL_SPONSORED_BY]->(b)";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }
	
	public void linkProposalPrimeSponsor() {
       String cypherQuery = " MATCH (a:Proposal), (b:Sponsor) WHERE a.prime_sponsor_code = b.sponsor_code MERGE (a)-[r:PROPOSAL_PRIME_SPONSORED_BY]->(b)    ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }	
	
	public void linkProposalUnit() {
       String cypherQuery = " MATCH (a:Proposal), (b:Unit) WHERE a.unit_number = b.unit_number MERGE (a)-[r:PROPOSAL_OWNED_BY]->(b) ";
       neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

   }
	
	public void importCOIDisclosure() {
		 String query = "select CONCAT('COI',t1.DISCLOSURE_ID) as ID, t1.DISCLOSURE_NUMBER,t1.PERSON_ID from coi_disclosure t1 where t1.VERSION_STATUS = 'ACTIVE'";
	     List<Disclosure> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	     Disclosure disclosure = Disclosure.builder()
						 .id(resultSet.getString("ID"))
						 .disclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"))
						 .personId(resultSet.getString("PERSON_ID"))
						 .build();      	
	            return disclosure;
	        });
	        coiRepository.saveAll(ls);
 }	

	public void importTravelDisclosure() {
		 String query = "SELECT CONCAT('TRV',t1.TRAVEL_DISCLOSURE_ID)  as ID,t1.TRAVEL_NUMBER, t1.PERSON_ID,t1.ENTITY_NUMBER FROM coi_travel_disclosure t1";
	        List<TravelDisclosure> ls = jdbcTemplate.query(query, (resultSet, rowNum) -> {
	        	TravelDisclosure disclosure = TravelDisclosure.builder()
						 .id(resultSet.getString("ID"))
						 .travelNumber(resultSet.getString("TRAVEL_NUMBER"))
						 .personId(resultSet.getString("PERSON_ID"))
						 .entityNumber(resultSet.getString("ENTITY_NUMBER"))						 					  
						 .build();      	
	            return disclosure;
	        });
	        travelRepository.saveAll(ls);
}	
	
	
	
	
	public void linkEntityToEntityRelationship() {

		String query = "SELECT \r\n"
				+ "t1.NODE_ID as FROM_ENTITY,\r\n"
				+ "UPPER(REPLACE(t2.DESCRIPTION, ' ', '_')) as REL, \r\n"
				+ "t1.ENTITY_NUMBER as TO_ENTITY\r\n"
				+ "FROM entity_relationship t1\r\n"
				+ "INNER JOIN entity_relationship_type t2 on t1.ENTITY_REL_TYPE_CODE = t2.ENTITY_REL_TYPE_CODE\r\n"
				+ "where t1.NODE_TYPE_CODE = 1\r\n"
				+ "\r\n"
				+ "UNION\r\n"
				+ "\r\n"
				+ "SELECT DISTINCT\r\n"
				+ "t1.NODE_ID as FROM_ENTITY,\r\n"
				+ "'SIBLINGS_FROM_THE_SAME_PARENT' as REL,\r\n"
				+ "t2.NODE_ID as TO_ENTITY\r\n"
				+ "FROM entity_relationship t1\r\n"
				+ "inner join entity_relationship t2 on t1.ENTITY_NUMBER = t2.ENTITY_NUMBER \r\n"
				+ "and t1.ENTITY_REL_TYPE_CODE = t2.ENTITY_REL_TYPE_CODE\r\n"
				+ "where t1.NODE_TYPE_CODE = 1\r\n"
				+ "and t1.NODE_ID < t2.NODE_ID";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (entity_1:Entity {entity_number: '" + resultSet.getString("FROM_ENTITY")+"' })\r\n"
						+ "				MATCH (entity_2:Entity {entity_number: '" +resultSet.getString("TO_ENTITY")+"' })\r\n"
						+ "				MERGE (entity_1)-[x:"+resultSet.getString("REL")+ "]->(entity_2)\r\n";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });
		 
   }

	
	public void linkPersonCOIDisclosure() {
		String cypherQuery = " MATCH (a:Person), (b:COIDisclosure) WHERE a.person_id = b.person_id MERGE (a)-[r:MY_FCOI_DISCLOSURE]->(b)";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	}


	public void linkEntityCOIDisclosure() {
		
		String query = "select distinct t1.DISCLOSURE_NUMBER,t2.ENTITY_NUMBER\r\n"
				+ "from coi_disclosure t1\r\n"
				+ "inner join coi_discl_ent_proj_details t2 on t1.DISCLOSURE_ID = t2.DISCLOSURE_ID\r\n"
				+ "where t1.VERSION_STATUS = 'ACTIVE'\r\n"
				+ "and t2.ENTITY_NUMBER is not null";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (a:Entity {entity_number: '" + resultSet.getString("ENTITY_NUMBER")+"' })\r\n"
						+ "				MATCH (b:COIDisclosure {disclosure_number: '" +resultSet.getString("DISCLOSURE_NUMBER")+"' })\r\n"
						+ "				MERGE (a)-[x:FINANCIALLY_SUPPORTED]->(b)\r\n";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });		
		
	}


	public void linkCOIDisclosureAward() {
		
		String query = "select distinct t1.DISCLOSURE_NUMBER,t3.AWARD_NUMBER AS MODULE_ITEM_KEY\r\n"
				+ "from coi_disclosure t1\r\n"
				+ "inner join coi_discl_ent_proj_details t2 on t1.DISCLOSURE_ID = t2.DISCLOSURE_ID\r\n"
				+ "inner join award t3 on t2.MODULE_ITEM_KEY = t3.AWARD_ID \r\n"
				+ "where t1.VERSION_STATUS = 'ACTIVE'\r\n"
				+ "and t2.MODULE_CODE  = 1";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (a:COIDisclosure {disclosure_number: '" + resultSet.getString("DISCLOSURE_NUMBER")+"' })\r\n"
						+ "				MATCH (b:Award {award_number: '" +resultSet.getString("MODULE_ITEM_KEY")+"' })\r\n"
						+ "				MERGE (a)-[x:LINKED_WITH_AWARD]->(b)\r\n";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });	
	}

	public void linkCOIDisclosureProposal() {
		
		String query = "select distinct t1.DISCLOSURE_NUMBER,t2.MODULE_ITEM_KEY \r\n"
				+ "from coi_disclosure t1\r\n"
				+ "inner join coi_discl_ent_proj_details t2 on t1.DISCLOSURE_ID = t2.DISCLOSURE_ID\r\n"
				+ "where t1.VERSION_STATUS = 'ACTIVE'\r\n"
				+ "and t2.MODULE_CODE  = 3";
	
				jdbcTemplate.query(query, (resultSet, rowNum) -> {
			
				String cypherQuery = "	MATCH (a:COIDisclosure {disclosure_number: '" + resultSet.getString("DISCLOSURE_NUMBER")+"' })\r\n"
						+ "				MATCH (b:Proposal {proposal_id: '" +resultSet.getString("MODULE_ITEM_KEY")+"' })\r\n"
						+ "				MERGE (a)-[x:LINKED_WITH_PROPOSAL]->(b)\r\n";
				
				neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			
				return null;
			 		
		        });			

	 }

	public void linkPersonTravelDisclosure() {
		String cypherQuery = "MATCH (a:Person), (b:TravelDisclosure) WHERE a.person_id = b.person_id MERGE (a)-[r:MY_TRAVEL_DISCLOSURE]->(b) ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	 }


	public void linkEntityTravelDisclosure() {
		String cypherQuery = "MATCH (a:Entity), (b:TravelDisclosure) WHERE a.entity_number = b.entity_number MERGE (a)-[r:TRAVEL_SPONSORED]->(b) ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	}	
}
