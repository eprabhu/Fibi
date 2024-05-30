package com.polus.fibi.graphconnect.coi.v1.dao;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.neo4j.core.Neo4jClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.COIEntity;
import com.polus.fibi.graphconnect.entity.Country;
import com.polus.fibi.graphconnect.entity.Person;
import com.polus.fibi.graphconnect.exceptions.CustomGraphException;
import com.polus.fibi.graphconnect.medusa.v1.dao.MedusaImportGraphDataDao;
import com.polus.fibi.graphconnect.repository.CountryRepository;
import com.polus.fibi.graphconnect.repository.EntityRepository;
import com.polus.fibi.graphconnect.repository.PersonRepository;



@Repository
public class COIImportGraphDataDao {

	@Autowired
	private EntityRepository entityRepository;

	@Autowired
	private CountryRepository countryRepository;

	@Autowired
	private PersonRepository personRepository;

	@Autowired
	@Lazy
	private MedusaImportGraphDataDao medusaImportGraphDataDao;

	@Autowired
	private Neo4jClient neo4jClient;

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Value("${spring.data.neo4j.database}")
	private String schema;

	private static final Logger logger = LoggerFactory.getLogger(COIImportGraphDataDao.class);

	public void importDataFromRDBMS() {
		deleteEverything();
		refreshDataFromRDBMS();
	}

	public void refreshDataFromRDBMS() {
		try {

			importCountry();
			importEntity();
			importPerson();
			linkEntityCountry();
			linkPersonCountry();
			linkPersontoEntityRelationship();
			linkEntityToEntityRelationship();

//			medusaImportGraphDataDao.refreshDataFromRDBMS();
		} catch (RuntimeException e) {
			throw new CustomGraphException("Runtime Exception in refreshDataFromRDBMS, error is " + e.getMessage());

		}

	}

	private void importCountry() {
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

	private void importEntity() {

		var start = Instant.now();
		logger.debug("--------- Import Entity: starts at ------------ " + start);

		String query = "SELECT  CONCAT('ENT',t1.ENTITY_ID) as ID, t1.ENTITY_NUMBER,\r\n"
				+ "t1.ENTITY_NAME as NAME, CASE \r\n"
				+ "        WHEN t1.IS_ACTIVE = 'Y' THEN 'Active'\r\n"
				+ "        WHEN t1.IS_ACTIVE = 'N' THEN 'Inactive' END as STATUS, \r\n"
				+ "t3.DESCRIPTION as TYPE, \r\n"
				+ "t5.DESCRIPTION as RISK,\r\n"
				+ "t1.COUNTRY_CODE, t4.COUNTRY_NAME, t1.WEB_URL,\r\n"
				+ "CASE\r\n"
				+ "    WHEN (select count(s1.ENTITY_NUMBER) from entity_relationship s1 where s1.ENTITY_NUMBER = t1.ENTITY_NUMBER and s1.NODE_TYPE_CODE = '2') > 0 THEN \"Y\"\r\n"
				+ "    ELSE 'N'\r\n"
				+ "END AS IS_SPONSOR\r\n"
				+ "FROM entity t1\r\n"
				+ "inner join entity_risk_category t5 on t1.RISK_CATEGORY_CODE = t5.RISK_CATEGORY_CODE\r\n"
				+ "inner join entity_type t3 on t1.entity_type_code = t3.entity_type_code\r\n"
				+ "left outer join country t4 on t1.COUNTRY_CODE = t4.COUNTRY_CODE\r\n"
				+ "WHERE t1.VERSION_NUMBER IN  (select MAX(s1.VERSION_NUMBER) from entity s1 where s1.ENTITY_NUMBER = t1.ENTITY_NUMBER and s1.VERSION_STATUS = 'ACTIVE' )";

		int pageSize = 4000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM entity", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(8);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchEntityData(query, pageSize, offset),
						executorService))
				.map(futureEntity -> futureEntity.thenAccept(this::saveEntityToNeo4j)).collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();

		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Entity: Total duration ------------ " + end);

	}

	private List<COIEntity> fetchEntityData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {

			COIEntity entity = COIEntity.builder().id(resultSet.getString("ID"))
					.entityNumber(resultSet.getString("ENTITY_NUMBER")).entityName(resultSet.getString("NAME"))
					.status(resultSet.getString("STATUS"))
					.isSponsor(resultSet.getString("IS_SPONSOR"))
					.type(resultSet.getString("TYPE"))
					.risk(resultSet.getString("RISK"))
					.countryName(resultSet.getString("COUNTRY_NAME")).countryCode(resultSet.getString("COUNTRY_CODE"))
					.build();
			return entity;

		});
	}

	private void saveEntityToNeo4j(List<COIEntity> entity) {
		entityRepository.saveAll(entity);
	}

	private void importPerson() {

		var start = Instant.now();
		logger.debug("--------- Import Person: starts at ------------ " + start);

/*
		String query = "select CONCAT('PER',PERSON_ID) AS ID, PERSON_ID,FULL_NAME,UNIT_NAME as HOME_UNIT,t1.COUNTRY_OF_CITIZENSHIP as COUNTRY_CODE, t4.COUNTRY_NAME,\r\n"
				+ "CASE   WHEN STATUS = 'A' THEN 'Active' ELSE 'Inactive' END AS STATUS \r\n" + "from person t1\r\n"
				+ "left outer join unit t2 on t1.home_unit = t2.unit_number\r\n"
				+ "left outer join country t4 on t1.COUNTRY_OF_CITIZENSHIP = t4.COUNTRY_CODE \r\n"
				+ "where PERSON_ID in ( select person_id from coi_disclosure UNION select person_id from coi_travel_disclosure UNION\r\n"
				+ "select person_id from person_entity)";
*/
				
		String query = """
				select CONCAT('PER',PERSON_ID) AS ID, PERSON_ID,FULL_NAME, USER_NAME, EMAIL_ADDRESS, PRIMARY_TITLE as DESIGNATION, UNIT_NAME as HOME_UNIT,t1.COUNTRY_OF_CITIZENSHIP as COUNTRY_CODE, t4.COUNTRY_NAME,
				CASE   WHEN STATUS = 'A' THEN 'Active' ELSE 'Inactive' END AS STATUS from person t1
				left outer join unit t2 on t1.home_unit = t2.unit_number
				left outer join country t4 on t1.COUNTRY_OF_CITIZENSHIP = t4.COUNTRY_CODE 
				where PERSON_ID in ( select person_id from coi_disclosure UNION select person_id from coi_travel_disclosure UNION
				select person_id from person_entity)
				OR
				t1.COUNTRY_OF_CITIZENSHIP is not null	
				""";	
						

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM person", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchPersonData(query, pageSize, offset),
						executorService))
				.map(futurePersons -> futurePersons.thenAccept(this::savePersonToNeo4j)).collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Person: Total duration ------------ " + end);

	}

	private List<Person> fetchPersonData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			Person person = Person.builder().id(resultSet.getString("ID")).personId(resultSet.getString("PERSON_ID"))
					.fullName(resultSet.getString("FULL_NAME")).homeUnit(resultSet.getString("HOME_UNIT"))
					.countryCode(resultSet.getString("COUNTRY_CODE")).countryName(resultSet.getString("COUNTRY_NAME"))
					.userName(resultSet.getString("USER_NAME")).emailAddress(resultSet.getString("EMAIL_ADDRESS"))
					.designation(resultSet.getString("DESIGNATION"))
					.status(resultSet.getString("STATUS")).build();
			return person;
		});
	}

	private void savePersonToNeo4j(List<Person> persons) {
		personRepository.saveAll(persons);
	}

	private void linkEntityCountry() {
		String cypherQuery = "MATCH (a:Entity) , (b:Country) WHERE a.country_code = b.country_code MERGE (a)-[r:COUNTRY]->(b)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	}

	private void linkPersonCountry() {
		String cypherQuery = "MATCH (a:Person) , (b:Country) WHERE a.country_code = b.country_code MERGE (a)-[r:CITIZEN_OF]->(b)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	}

	private void linkPersontoEntityRelationship() {

		String query = """
					select
					z0.PERSON_ID,z0.ENTITY_NUMBER , z0.ENTITY_NAME,
					GROUP_CONCAT(DISTINCT t6.disclosure_type SEPARATOR ',') AS CATEGORY,
					GROUP_CONCAT(DISTINCT  z0.REL  SEPARATOR ',') AS REL
					from
					(
						select
						t0.PERSON_ID,t0.ENTITY_NUMBER, t5.ENTITY_NAME ,
						CONCAT(t3.DESCRIPTION, '[', GROUP_CONCAT(DISTINCT t4.DESCRIPTION SEPARATOR ','), ']') AS REL
						from person_entity t0
						inner join person_entity_relationship t1 on t1.PERSON_ENTITY_ID  = t0.PERSON_ENTITY_ID
						inner join VALID_PERSON_ENTITY_REL_TYPE t2 on t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
						inner join coi_disclosure_type t3 on t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
						inner join person_entity_rel_type t4 on t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE
						inner join entity t5 on t5.ENTITY_ID = t0.ENTITY_ID
						group by t0.PERSON_ID,t0.ENTITY_NUMBER , t5.ENTITY_NAME  ,t3.DESCRIPTION
					) z0
					left outer join (

							select
							PERSON_ID,
							ENTITY_NUMBER,
							'TRAVEL' as disclosure_type
							from coi_travel_disclosure

					        UNION ALL

							select t20.PERSON_ID,t21.ENTITY_NUMBER, 'FCOI' as disclosure_type
							from coi_disclosure t20
							inner join coi_discl_ent_proj_details t21 on t20.DISCLOSURE_ID = t21.DISCLOSURE_ID
							where t20.FCOI_TYPE_CODE = '1'
							and t21.ENTITY_NUMBER is not null
							group by t20.PERSON_ID,t21.ENTITY_NUMBER

					) t6 on t6.PERSON_ID = z0.PERSON_ID and t6.ENTITY_NUMBER = z0.ENTITY_NUMBER

					group by z0.PERSON_ID,z0.ENTITY_NUMBER,z0.ENTITY_NAME




				""";

		jdbcTemplate.query(query, (resultSet, rowNum) -> {

			String cypherQuery = "	MATCH (person:Person {person_id: '" + resultSet.getString("PERSON_ID") + "' })\r\n"
					+ "				MATCH (entity:Entity {entity_number: '" + resultSet.getString("ENTITY_NUMBER")
					+ "' })\r\n" + " MERGE (person)-[x:ASSOCIATED_ENTITIES]->(entity)\r\n"
					+ "				SET x.Category = '" + resultSet.getString("CATEGORY") + "' ,\r\n"
					+ "		x.Relationship_Info = '" + resultSet.getString("REL") + "'	  ";

			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

			return null;

		});

	}

	private void linkEntityToEntityRelationship() {

		String query = """

						SELECT
						t1.ENTITY_NUMBER as FROM_ENTITY,
						t2.DESCRIPTION as REL_TYPE,
						t1.NODE_ID as TO_ENTITY
						FROM entity_relationship t1
						INNER JOIN entity_relationship_type t2 on t1.ENTITY_REL_TYPE_CODE = t2.ENTITY_REL_TYPE_CODE
						where t1.NODE_TYPE_CODE = 1

				""";

		jdbcTemplate.query(query, (resultSet, rowNum) -> {

			String cypherQuery = " MATCH (entity_1:Entity {entity_number: '" + resultSet.getString("FROM_ENTITY")
					+ "' })\r\n" + " MATCH (entity_2:Entity {entity_number: '" + resultSet.getString("TO_ENTITY")
					+ "' })\r\n" + " MERGE (entity_1)-[x:AFFILIATED_ENTITIES]->(entity_2) "
					+ "				 SET x.Relationship_type = '" + resultSet.getString("REL_TYPE") + "' ";
			;

			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

			return null;

		});

	}

	private void deleteEverything() {
		deleteCOINodes();
		deleteCOIRelationships();
//		String cypherQuery = "MATCH (n) DETACH DELETE n";
//		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();

	}

	private void deleteCOIRelationships() {
		String cypherQuery = "MATCH ()-[r:COUNTRY | CITIZEN_OF | ASSOCIATED_ENTITIES | AFFILIATED_ENTITIES]->() DELETE r";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void deleteCOINodes() {
		String cypherQuery = "MATCH (n) WHERE n:Country OR n:Person OR n:Entity DETACH DELETE n";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}
}
