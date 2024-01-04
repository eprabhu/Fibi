package com.polus.fibi.graphconnect.medusa.v1.dao;

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
import org.springframework.data.neo4j.core.Neo4jClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import com.polus.fibi.graphconnect.entity.ACProtocol;
import com.polus.fibi.graphconnect.entity.Agreement;
import com.polus.fibi.graphconnect.entity.Award;
import com.polus.fibi.graphconnect.entity.GrantCall;
import com.polus.fibi.graphconnect.entity.IRBProtocol;
import com.polus.fibi.graphconnect.entity.InstituteProposal;
import com.polus.fibi.graphconnect.entity.Person;
import com.polus.fibi.graphconnect.entity.Proposal;
import com.polus.fibi.graphconnect.entity.ServiceRequest;
import com.polus.fibi.graphconnect.exceptions.CustomGraphException;
import com.polus.fibi.graphconnect.repository.ACProtocolRepository;
import com.polus.fibi.graphconnect.repository.AgreementRepository;
import com.polus.fibi.graphconnect.repository.AwardRepository;
import com.polus.fibi.graphconnect.repository.GrantCallRepository;
import com.polus.fibi.graphconnect.repository.IRBProtocolRepository;
import com.polus.fibi.graphconnect.repository.InstituteProposalRepository;
import com.polus.fibi.graphconnect.repository.PersonRepository;
import com.polus.fibi.graphconnect.repository.ProposalRepository;
import com.polus.fibi.graphconnect.repository.ServiceRequestRepository;

@Repository
public class MedusaImportGraphDataDao {

	@Autowired
	private PersonRepository personRepository;

	@Autowired
	private InstituteProposalRepository instituteProposalRepository;

	@Autowired
	private ProposalRepository proposalRepository;

	@Autowired
	private AwardRepository awardRepository;

	@Autowired
	private GrantCallRepository grantCallRepository;

	@Autowired
	private ACProtocolRepository acProtocolRepository;

	@Autowired
	private IRBProtocolRepository irbProtocolRepository;

	@Autowired
	private ServiceRequestRepository serviceRequestRepository;

	@Autowired
	private AgreementRepository agreementRepository;

	@Autowired
	private Neo4jClient neo4jClient;

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Value("${spring.data.neo4j.database}")
	private String schema;

	private static final Logger logger = LoggerFactory.getLogger(MedusaImportGraphDataDao.class);

	public void importDataFromRDBMS() {
		deleteEverything();
		refreshDataFromRDBMS();
	}

	private void deleteEverything() {
		deleteMedusaNodes();
		deleteMedusaRelationships();
//		String cypherQuery = "MATCH (n) DETACH DELETE n";
//		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	public void refreshDataFromRDBMS() {
		try {
			importPerson();
			importGrantCall();
			importProposal();
			importInstituteProposal();
			importAward();
			importServiceRequest();
			importAgreements();
			importACProtocol();
			importIRBProtocol();
			linkGrantCallToDevProposalRelationship();
			linkGrantCallToAwardRelationship();
			linkGrantCallToSRRelationship();
			linkDevProposalToIPRelationship();
			linkDevProposalToSRRelationship();
			linkDevProposalToAgreementRelationship();
			linkIPToSRRelationship();
			linkIPToAgreementRelationship();
			linkIPToAwardRelationship();
			linkAwardToAgreementRelationship();
			linkAwardToSRRelationship();
			linkAgreementToSRRelationship();
			linkAgreementToAgreementRelationship();
			linkSRToSRRelationship();
			linkAwardToChildAwardRelationship();
		} catch (RuntimeException e) {
			throw new CustomGraphException("Runtime Exception in refreshDataFromRDBMS, error is " + e.getMessage());
		}
	}

	private void linkAwardToChildAwardRelationship() {
		String cypherQuery = "MATCH (a:Award), (b:Award) "
				+ "WHERE LEFT(b.award_number, SIZE(b.award_number) - 1) = LEFT(a.award_number, SIZE(a.award_number) - 1) "
				+ "AND TOINTEGER(RIGHT(a.award_number, 1)) = 1 "
				+ "AND id(a) <> id(b) "
				+ "MERGE (a)-[r:CHILD_AWARDS]->(b) ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
		
	}

	private void linkSRToSRRelationship() {
		String cypherQuery = "MATCH (a:ServiceRequest) , (b:ServiceRequest) WHERE a.service_request_id = b.module_item_key AND b.module_code = '20' MERGE (a)-[r:SR_TO_SR]->(b)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkAgreementToAgreementRelationship() {
		String query = """
						SELECT
						t1.AGREEMENT_REQUEST_ID,
						t1.MODULE_ITEM_KEY
						FROM agreement_association_link t1
						where t1.MODULE_CODE = 13
				""";
		jdbcTemplate.query(query, (resultSet, rowNum) -> {
			String cypherQuery = " MATCH (a1:Agreement {agreement_request_id: '"
					+ resultSet.getString("AGREEMENT_REQUEST_ID") + "' })\r\n" + " MATCH (a2:Agreement {agreement_request_id: '"
					+ resultSet.getString("MODULE_ITEM_KEY") + "' })\r\n"
					+ " MERGE (a1)-[x:AGREEMENT_TO_AGREEMENT]->(a2) ";
			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			return null;
		});
	}

	private void linkAgreementToSRRelationship() {
		String cypherQuery = "MATCH (a:Agreement) , (b:ServiceRequest) WHERE a.agreement_request_id = b.module_item_key AND b.module_code = '13' MERGE (b)-[r:AGREEMENT_TO_SR]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkAwardToSRRelationship() {
		String cypherQuery = "MATCH (a:Award) , (b:ServiceRequest) WHERE a.award_id = b.module_item_key AND b.module_code = '1' MERGE (b)-[r:RELATES_TO]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkAwardToAgreementRelationship() {
		String query = """
						SELECT
						t1.AGREEMENT_REQUEST_ID,
						t1.MODULE_ITEM_KEY
						FROM agreement_association_link t1
						where t1.MODULE_CODE = 1
				""";
		jdbcTemplate.query(query, (resultSet, rowNum) -> {
			String cypherQuery = " MATCH (agreement:Agreement {agreement_request_id: '"
					+ resultSet.getString("AGREEMENT_REQUEST_ID") + "' })\r\n"
					+ " MATCH (award:Award {award_id: '" + resultSet.getString("MODULE_ITEM_KEY") + "' })\r\n"
					+ " MERGE (award)-[x:ASSOCIATED_WITH]->(agreement) ";
			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			return null;
		});
	}

	private void linkIPToAwardRelationship() {
		String query = """
						SELECT
						t1.AWARD_ID,
						t1.PROPOSAL_ID
						FROM award_funding_proposals t1
				""";

		jdbcTemplate.query(query, (resultSet, rowNum) -> {

			String cypherQuery = " MATCH (award:Award {award_id: '" + resultSet.getString("AWARD_ID") + "' })\r\n"
					+ " MATCH (ip:InstituteProposal {proposal_id: '" + resultSet.getString("PROPOSAL_ID") + "' })\r\n"
					+ " MERGE (ip)-[x:FUNDED_BY]->(award) ";
			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			return null;
		});
	}

	private void linkIPToAgreementRelationship() {
		String query = """
						SELECT
						t1.AGREEMENT_REQUEST_ID,
						t1.MODULE_ITEM_KEY
						FROM agreement_association_link t1
						where t1.MODULE_CODE = 2
				""";

		jdbcTemplate.query(query, (resultSet, rowNum) -> {

			String cypherQuery = " MATCH (agreement:Agreement {agreement_request_id: '"
					+ resultSet.getString("AGREEMENT_REQUEST_ID") + "' })\r\n"
					+ " MATCH (ip:InstituteProposal {proposal_Id: '" + resultSet.getString("MODULE_ITEM_KEY") + "' })\r\n"
					+ " MERGE (ip)-[x:ASSOCIATED_WITH]->(agreement) ";
			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			return null;
		});
	}

	private void linkIPToSRRelationship() {
		String cypherQuery = "MATCH (a:InstituteProposal) , (b:ServiceRequest) WHERE a.proposal_id = b.module_item_key AND b.module_code = '2' MERGE (b)-[r:RELATES_TO]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkDevProposalToAgreementRelationship() {
		String query = """
						SELECT
						t1.AGREEMENT_REQUEST_ID,
						t1.MODULE_ITEM_KEY
						FROM agreement_association_link t1
						where t1.MODULE_CODE = 3
				""";

		jdbcTemplate.query(query, (resultSet, rowNum) -> {

			String cypherQuery = " MATCH (agreement:Agreement {agreement_request_id: '" + resultSet.getString("AGREEMENT_REQUEST_ID")
					+ "' })\r\n" + " MATCH (proposal:Proposal {proposal_id: '" + resultSet.getString("MODULE_ITEM_KEY")
					+ "' })\r\n" + " MERGE (proposal)-[x:ASSOCIATED_WITH]->(agreement) ";
			neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
			return null;
		});
	}

	private void linkDevProposalToSRRelationship() {
		String cypherQuery = "MATCH (a:Proposal) , (b:ServiceRequest) WHERE a.proposal_Id = b.module_item_key AND b.module_code = '3' MERGE (b)-[r:PD_TO_SR]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkDevProposalToIPRelationship() {
		String cypherQuery = "MATCH (a:Proposal) , (b:InstituteProposal) WHERE a.ip_number = b.proposal_number MERGE (a)-[r:APPROVED_AS]->(b)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkGrantCallToSRRelationship() {
		String cypherQuery = "MATCH (a:GrantCall) , (b:ServiceRequest) WHERE a.grant_call_id = b.module_item_key AND b.module_code = '15' MERGE (b)-[r:RELATES_TO]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkGrantCallToAwardRelationship() {
		String cypherQuery = "MATCH (a:GrantCall) , (b:Award) WHERE a.grant_call_id = b.grant_header_id MERGE (a)-[r:RESULTS_IN]->(b)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void linkGrantCallToDevProposalRelationship() {
		String cypherQuery = "MATCH (a:Proposal) , (b:GrantCall) WHERE a.grant_header_id = b.grant_call_id MERGE (b)-[r:INITIATES]->(a)  ";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

/*----------------------------------------------------------------------------Import IRB Protocol--------------------------------------------------------------------------------------------------------------*/

	private void importIRBProtocol() {
		var start = Instant.now();
		logger.debug("--------- Import IRB Protocol: starts at ------------ " + start);
		String query = """
				select CONCAT('IRB',PROTOCOL_ID) AS ID, PROTOCOL_ID, PROTOCOL_NUMBER, TITLE, FDA_APPLICATION_NUMBER from irb_protocol t1
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM irb_protocol", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchIRBProtocolData(query, pageSize, offset),
						executorService))
				.map(futureIRBProtocols -> futureIRBProtocols.thenAccept(this::saveIRBProtocolToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import IRB Protocol: Total duration ------------ " + end);
	}

	private List<IRBProtocol> fetchIRBProtocolData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			IRBProtocol irbProtocol = IRBProtocol.builder().id(resultSet.getString("ID"))
					.protocolId(resultSet.getString("PROTOCOL_ID")).protocolNumber(resultSet.getString("PROTOCOL_NUMBER"))
					.title(resultSet.getString("TITLE"))
					.fdaApplicationNumber(resultSet.getString("FDA_APPLICATION_NUMBER")).build();
			return irbProtocol;
		});
	}

	private void saveIRBProtocolToNeo4j(List<IRBProtocol> irbProtocols) {
		irbProtocolRepository.saveAll(irbProtocols);
	}
/*----------------------------------------------------------------------------Import IRB Protocol--------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------Import AC Protocol--------------------------------------------------------------------------------------------------------------*/

	private void importACProtocol() {
		var start = Instant.now();
		logger.debug("--------- Import AC Protocol: starts at ------------ " + start);
		String query = """
				select CONCAT('AC',PROTOCOL_ID) AS ID, PROTOCOL_ID, PROTOCOL_NUMBER, TITLE, PURPOSE_OF_STUDY, FDA_APPLICATION_NUMBER from ac_protocol t1
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM ac_protocol", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchACProtocolData(query, pageSize, offset),
						executorService))
				.map(futureACProtocols -> futureACProtocols.thenAccept(this::saveACProtocolToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import AC Protocol: Total duration ------------ " + end);
	}

	private List<ACProtocol> fetchACProtocolData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			ACProtocol acProtocol = ACProtocol.builder().id(resultSet.getString("ID"))
					.protocolId(resultSet.getString("PROTOCOL_ID")).protocolNumber(resultSet.getString("PROTOCOL_NUMBER"))
					.title(resultSet.getString("TITLE")).purposeOfStudy(resultSet.getString("PURPOSE_OF_STUDY"))
					.fdaApplicationNumber(resultSet.getString("FDA_APPLICATION_NUMBER")).build();
			return acProtocol;
		});
	}

	private void saveACProtocolToNeo4j(List<ACProtocol> acProtocols) {
		acProtocolRepository.saveAll(acProtocols);
	}

/*----------------------------------------------------------------------------Import AC Protocol--------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------Import Agreement--------------------------------------------------------------------------------------------------------------*/

	private void importAgreements() {
		var start = Instant.now();
		logger.debug("--------- Import Agreement: starts at ------------ " + start);
		String query = """
				SELECT CONCAT('AGR', t1.AGREEMENT_REQUEST_ID) AS ID, t1.AGREEMENT_REQUEST_ID, t1.TITLE, t1.REMARKS, t1.REQUESTOR_NAME, t1.SUBMIT_USER, t1.AGREEMENT_SEQUENCE_STATUS, t1.UNIT_NUMBER, t1.UNIT_NAME,
				t2.SPONSOR_NAME, t2.PRINCIPAL_PERSON_FULL_NAME, t2.AGREEMENT_STATUS
				from agreement_header t1 
				inner join agreement_v t2 on t1.AGREEMENT_REQUEST_ID = t2.AGREEMENT_REQUEST_ID
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM agreement_header", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchAgreementData(query, pageSize, offset),
						executorService))
				.map(futureAgreements -> futureAgreements.thenAccept(this::saveAgreementToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Agreement: Total duration ------------ " + end);
	}

	private List<Agreement> fetchAgreementData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			Agreement agreement = Agreement.builder().id(resultSet.getString("ID"))
					.agreementRequestId(resultSet.getString("AGREEMENT_REQUEST_ID")).title(resultSet.getString("TITLE"))
					.remarks(resultSet.getString("REMARKS"))
					.requestorName(resultSet.getString("REQUESTOR_NAME"))
					.submitUser(resultSet.getString("SUBMIT_USER"))
					.agreementSequenceStatus(resultSet.getString("AGREEMENT_SEQUENCE_STATUS"))
					.unitName(resultSet.getString("UNIT_NAME"))
					.unitNumber(resultSet.getString("UNIT_NUMBER"))
					.sponsorName(resultSet.getString("SPONSOR_NAME"))
					.principalPersonFullName(resultSet.getString("PRINCIPAL_PERSON_FULL_NAME"))
					.agreementStatus(resultSet.getString("AGREEMENT_STATUS"))
					.build();
			return agreement;
		});
	}

	private void saveAgreementToNeo4j(List<Agreement> agreements) {
		agreementRepository.saveAll(agreements);
	}
/*----------------------------------------------------------------------------Import Agreement--------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------Import Service Request--------------------------------------------------------------------------------------------------------------*/

	private void importServiceRequest() {
		var start = Instant.now();
		logger.debug("--------- Import Service Request: starts at ------------ " + start);
		String query = """
				select CONCAT('SR',T1.SR_HEADER_ID) AS ID, T1.SR_HEADER_ID AS SR_HEADER_ID ,T1.SUBJECT AS SUBJECT ,T2.DESCRIPTION AS STATUS, T3.DESCRIPTION AS TYPE ,T1.UNIT_NUMBER AS UNIT_NUMBER, 
				T4.UNIT_NAME as UNIT_NAME , T1.REPORTER_PERSON_ID AS REPORTER_PERSON_ID, T5.FULL_NAME as REPORTED_BY, DATE_FORMAT(T1.CREATE_TIMESTAMP, '%m/%d/%Y') AS CREATE_TIMESTAMP, T1.MODULE_CODE, T1.MODULE_ITEM_KEY 
				FROM SR_HEADER T1
				LEFT JOIN SR_STATUS T2 
				ON T2.STATUS_CODE = T1.STATUS_CODE
				LEFT JOIN SR_TYPE T3
				ON T3.TYPE_CODE = T1.TYPE_CODE
				LEFT JOIN UNIT T4
				ON T4.UNIT_NUMBER = T1.UNIT_NUMBER
				LEFT JOIN PERSON T5
				ON T5.PERSON_ID = T1.REPORTER_PERSON_ID
				WHERE T1.STATUS_CODE <> 1
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM sr_header", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchServiceRequestData(query, pageSize, offset),
						executorService))
				.map(futureServiceRequests -> futureServiceRequests.thenAccept(this::saveServiceRequestToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Service Request: Total duration ------------ " + end);
	}

	private List<ServiceRequest> fetchServiceRequestData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			ServiceRequest serviceRequest = ServiceRequest.builder().id(resultSet.getString("ID"))
					.srHeaderId(resultSet.getString("SR_HEADER_ID"))
					.subject(resultSet.getString("SUBJECT"))
					.status(resultSet.getString("STATUS"))
					.reporterPersonId(resultSet.getString("REPORTER_PERSON_ID"))
					.type(resultSet.getString("TYPE"))
					.unitNumber(resultSet.getString("UNIT_NUMBER"))
					.unitName(resultSet.getString("UNIT_NAME"))
					.reportedBy(resultSet.getString("REPORTED_BY"))
					.createTimestamp(resultSet.getString("CREATE_TIMESTAMP"))
					.moduleCode(resultSet.getString("MODULE_CODE"))
					.moduleItemKey(resultSet.getString("MODULE_ITEM_KEY"))
					.build();
			return serviceRequest;
		});
	}

	private void saveServiceRequestToNeo4j(List<ServiceRequest> serviceRequests) {
		serviceRequestRepository.saveAll(serviceRequests);
	}
/*----------------------------------------------------------------------------Import Service Request--------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------Import Grant Call--------------------------------------------------------------------------------------------------------------*/
	private void importGrantCall() {
		var start = Instant.now();
		logger.debug("--------- Import Grant Call: starts at ------------ " + start);
		String query = """
				select CONCAT('GC',t1.GRANT_HEADER_ID) AS ID, t1.GRANT_HEADER_ID, t1.NAME, t1.DESCRIPTION, t1.GRANT_THEME, t1.MAX_BUDGET, t2.DESCRIPTION AS STATUS, t3.DESCRIPTION AS GRANT_TYPE, 
				t4.DESCRIPTION AS FUNDING_SOURCE, t5.DESCRIPTION AS SPONSOR, t1.HOME_UNIT_NAME, t1.HOME_UNIT_NUMBER, DATE_FORMAT(t1.OPENING_DATE, '%m/%d/%Y') AS OPENING_DATE, DATE_FORMAT(t1.CLOSING_DATE, '%m/%d/%Y') AS CLOSING_DATE 
				from grant_call_header t1
				left join grant_call_status t2 on t1.GRANT_STATUS_CODE = t2.GRANT_STATUS_CODE
				left join grant_call_type t3 on t1.GRANT_TYPE_CODE = t3.GRANT_TYPE_CODE
				left join SPONSOR_FUNDING_SCHEME t4 on t1.FUNDING_SCHEME_ID = t4.FUNDING_SCHEME_ID
				left join SPONSOR_TYPE t5 on t1.SPONSOR_TYPE_CODE = t5.SPONSOR_TYPE_CODE
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM grant_call_header", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchGrantCallData(query, pageSize, offset),
						executorService))
				.map(futureGrantCalls -> futureGrantCalls.thenAccept(this::saveGrantCallToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Grant Call: Total duration ------------ " + end);
	}

	private List<GrantCall> fetchGrantCallData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			GrantCall grantCall = GrantCall.builder().id(resultSet.getString("ID"))
					.grantCallId(resultSet.getString("GRANT_HEADER_ID")).grantCallName(resultSet.getString("NAME"))
					.description(resultSet.getString("DESCRIPTION")).grantTheme(resultSet.getString("GRANT_THEME"))
					.maxBudget(resultSet.getString("MAX_BUDGET"))
					.status(resultSet.getString("STATUS"))
					.grantType(resultSet.getString("GRANT_TYPE"))
					.fundingSource(resultSet.getString("FUNDING_SOURCE"))
					.sponsor(resultSet.getString("SPONSOR"))
					.homeUnitName(resultSet.getString("HOME_UNIT_NAME"))
					.homeUnitNumber(resultSet.getString("HOME_UNIT_NUMBER"))
					.openingDate(resultSet.getString("OPENING_DATE"))
					.closingDate(resultSet.getString("CLOSING_DATE"))
					.build();
			return grantCall;
		});
	}

	private void saveGrantCallToNeo4j(List<GrantCall> grantCalls) {
		grantCallRepository.saveAll(grantCalls);
	}

/*----------------------------------------------------------------------------Import Grant Call--------------------------------------------------------------------------------------------------------------*/
	
/*----------------------------------------------------------------------------Import Institute Proposal--------------------------------------------------------------------------------------------------------------*/

	private void importInstituteProposal() {
		var start = Instant.now();
		logger.debug("--------- Import Institute Proposal: starts at ------------ " + start);
		String query = """
				select CONCAT('IP',T1.PROPOSAL_ID) AS ID,
				T1.PROPOSAL_ID,
                T1.PROPOSAL_NUMBER,
                T1.TITLE,
                T1.HOME_UNIT_NUMBER,
                DATE_FORMAT(T1.START_DATE, '%m/%d/%Y') AS START_DATE,    
                DATE_FORMAT(T1.END_DATE, '%m/%d/%Y') AS END_DATE, 
                T3.UNIT_NAME,
                T1.SPONSOR_PROPOSAL_NUMBER,
                T4.DESCRIPTION AS ACTIVITY_TYPE,
                T6.SPONSOR_CODE AS PRIME_SPONSOR_CODE,
                T6.ACRONYM AS PRIME_SPONSOR_ACRONYM,
                T6.SPONSOR_NAME AS PRIME_SPONSOR,
                T7.DESCRIPTION AS STATUS,
                T8.FULL_NAME AS INVESTIGATOR,
                T9.DESCRIPTION AS PROPOSAL_TYPE,
                T11.SPONSOR_CODE,
                T11.ACRONYM ,
                T11.SPONSOR_NAME AS SPONSOR
            FROM PROPOSAL T1 
            LEFT OUTER JOIN UNIT T3 ON T1.HOME_UNIT_NUMBER = T3.UNIT_NUMBER
            LEFT OUTER JOIN ACTIVITY_TYPE T4 ON T1.ACTIVITY_TYPE_CODE = T4.ACTIVITY_TYPE_CODE
            LEFT OUTER JOIN SPONSOR T6 ON T1.PRIME_SPONSOR_CODE = T6.SPONSOR_CODE
            LEFT OUTER JOIN PROPOSAL_STATUS T7 ON T1.STATUS_CODE = T7.STATUS_CODE
            LEFT OUTER JOIN PROPOSAL_PERSONS T8 ON T1.PROPOSAL_ID = T8.PROPOSAL_ID  AND T8.PROP_PERSON_ROLE_ID = 3
            LEFT OUTER JOIN PROPOSAL_TYPE T9 ON T1.TYPE_CODE = T9.TYPE_CODE
            LEFT OUTER JOIN SPONSOR T11 ON T1.SPONSOR_CODE = T11.SPONSOR_CODE
				""";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM proposal", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchInstituteProposalData(query, pageSize, offset),
						executorService))
				.map(futurePersons -> futurePersons.thenAccept(this::saveInstituteProposalToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Institute Proposal: Total duration ------------ " + end);

	}

	private List<InstituteProposal> fetchInstituteProposalData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			InstituteProposal instituteProposal = InstituteProposal.builder()
					.id(resultSet.getString("ID"))
					.proposalId(resultSet.getString("PROPOSAL_ID"))
					.proposalNumber(resultSet.getString("PROPOSAL_NUMBER"))
					.title(resultSet.getString("TITLE"))
					.homeUnitNumber(resultSet.getString("HOME_UNIT_NUMBER"))
					.startDate(resultSet.getString("START_DATE"))
					.endDate(resultSet.getString("END_DATE"))
					.unitName(resultSet.getString("UNIT_NAME"))
					.sponsorProposalNumber(resultSet.getString("SPONSOR_PROPOSAL_NUMBER"))
					.activityType(resultSet.getString("ACTIVITY_TYPE"))
					.primeSponsorCode(resultSet.getString("PRIME_SPONSOR_CODE"))
					.primeSponsorAcronym(resultSet.getString("PRIME_SPONSOR_ACRONYM"))
					.primeSponsor(resultSet.getString("PRIME_SPONSOR"))
					.investigator(resultSet.getString("INVESTIGATOR"))
					.status(resultSet.getString("STATUS"))
					.proposalType(resultSet.getString("PROPOSAL_TYPE"))
					.sponsorCode(resultSet.getString("SPONSOR_CODE"))
					.acronym(resultSet.getString("ACRONYM"))
					.sponsor(resultSet.getString("SPONSOR"))
					.build();
			return instituteProposal;
		});
	}

	

	private void saveInstituteProposalToNeo4j(List<InstituteProposal> instituteProposals) {
		instituteProposalRepository.saveAll(instituteProposals);
	}
	
/*----------------------------------------------------------------------------Import Person--------------------------------------------------------------------------------------------------------------*/
	private void importPerson() {
		var start = Instant.now();
		logger.debug("--------- Import Person: starts at ------------ " + start);
		String query = """
				select CONCAT('PER',PERSON_ID) AS ID, PERSON_ID,FULL_NAME, USER_NAME, EMAIL_ADDRESS, PRIMARY_TITLE as DESIGNATION, UNIT_NAME as HOME_UNIT,t1.COUNTRY_OF_CITIZENSHIP as COUNTRY_CODE, t4.COUNTRY_NAME,
				CASE   WHEN STATUS = 'A' THEN 'Active' ELSE 'Inactive' END AS STATUS from person t1
				left outer join unit t2 on t1.home_unit = t2.unit_number
				left outer join country t4 on t1.COUNTRY_OF_CITIZENSHIP = t4.COUNTRY_CODE
				where t1.COUNTRY_OF_CITIZENSHIP is not null
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
					.designation(resultSet.getString("DESIGNATION")).status(resultSet.getString("STATUS")).build();
			return person;
		});
	}

	private void savePersonToNeo4j(List<Person> persons) {
		personRepository.saveAll(persons);
	}

/*----------------------------------------------------------------------------Import Person--------------------------------------------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------Import Award--------------------------------------------------------------------------------------------------------------*/


	public void importAward() {

		var start = Instant.now();
		logger.debug("--------- Import Award: starts at ------------ " + start);
		String query = "SELECT \r\n"
				+ "distinct CONCAT('AWD',t1.AWARD_ID) AS ID,t1.AWARD_ID, t1.ACCOUNT_NUMBER, t1.ANTICIPATED_TOTAL_AMOUNT, t1.AWARD_STATUS, t1.AWARD_NUMBER,t1.TITLE,t1.AWARD_TYPE,t1.LEAD_UNIT_NUMBER as UNIT_NUMBER,t1.LEAD_UNIT,t1.SPONSOR_CODE,t1.SPONSOR_NAME,t1.PRIME_SPONSOR_CODE,t1.PRIME_SPONSOR_NAME,\r\n"
				+ "DATE_FORMAT(t1.BEGIN_DATE, '%m/%d/%Y') as START_DATE, t2.GRANT_HEADER_ID, \r\n"
				+ "DATE_FORMAT(t1.FINAL_EXPIRATION_DATE, '%m/%d/%Y') as END_DATE,\r\n" + "t1.PI_NAME\r\n" + "FROM award_v t1 inner join award t2 on t1.AWARD_ID = t2.AWARD_ID";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM award_v", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchAwardData(query, pageSize, offset),
						executorService))
				.map(futureAwards -> futureAwards.thenAccept(this::saveAwardToNeo4j)).collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import Award: Total duration ------------ " + end);
	}

	private List<Award> fetchAwardData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			Award award = Award.builder().id(resultSet.getString("ID")).awardId(resultSet.getString("AWARD_ID"))
					.awardNumber(resultSet.getString("AWARD_NUMBER"))
					.accountNumber(resultSet.getString("ACCOUNT_NUMBER"))
					.anticipatedTotalAmount(resultSet.getString("ANTICIPATED_TOTAL_AMOUNT"))
					.awardStatus(resultSet.getString("AWARD_STATUS"))
					.title(resultSet.getString("TITLE")).startDate(resultSet.getString("START_DATE"))
					.endDate(resultSet.getString("END_DATE")).sponsorCode(resultSet.getString("SPONSOR_CODE"))
					.sponsorName(resultSet.getString("SPONSOR_NAME"))
					.primeSponsorCode(resultSet.getString("PRIME_SPONSOR_CODE"))
					.primeSponsorName(resultSet.getString("PRIME_SPONSOR_NAME"))
					.unitNumber(resultSet.getString("UNIT_NUMBER")).leadUnitName(resultSet.getString("LEAD_UNIT"))
					.grantHeaderId(resultSet.getString("GRANT_HEADER_ID"))
					.awardType(resultSet.getString("AWARD_TYPE")).piName(resultSet.getString("PI_NAME")).build();
			return award;
		});
	}

	private void saveAwardToNeo4j(List<Award> awards) {
		awardRepository.saveAll(awards);
	}
	
/*----------------------------------------------------------------------------Import Award--------------------------------------------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------Import eps_proposal--------------------------------------------------------------------------------------------------------------*/

	public void importProposal() {

		var start = Instant.now();
		logger.debug("--------- Import eps_proposal: starts at ------------ " + start);
		String query = "SELECT distinct CONCAT('EPS',t1.PROPOSAL_ID) AS ID, t1.PROPOSAL_ID, t1.LEAD_UNIT_NUMBER, t1.PROPOSAL_STATUS, t1.TITLE, DATE_FORMAT(t1.START_DATE, '%m/%d/%Y') as START_DATE ,\r\n"
				+ "DATE_FORMAT(t1.END_DATE, '%m/%d/%Y') as END_DATE,\r\n" + "t2.SPONSOR_CODE,\r\n"
				+ "t1.SPONSOR_NAME,\r\n" + "t2.PRIME_SPONSOR_CODE,\r\n" + "t3.SPONSOR_NAME as PRIME_SPONSOR_NAME,\r\n"
				+ "t1.LEAD_UNIT_NUMBER as UNIT_NUMBER, t2.GRANT_HEADER_ID, t2.IP_NUMBER, \r\n"
				+ "t1.LEAD_UNIT_NAME, t1.TYPE_OF_FUNDING_AGENCY, t1.PI_NAME\r\n" + "FROM eps_proposal_v t1\r\n"
				+ "inner join eps_proposal t2 on t1.PROPOSAL_ID = t2.PROPOSAL_ID\r\n"
				+ "left outer join sponsor t3 on t2.PRIME_SPONSOR_CODE = t3.SPONSOR_CODE\r\n" + "";

		int pageSize = 10000; // Number of records to fetch in each batch

		// Get the total count of records for pagination
		int totalCount = jdbcTemplate.queryForObject("SELECT COUNT(*) FROM eps_proposal_v", Integer.class);

		// Create a thread pool for parallel processing
		ExecutorService executorService = Executors.newFixedThreadPool(5);

		// Create batches using pagination and process them in parallel
		List<CompletableFuture<Void>> futureList = Stream.iterate(0, i -> i + pageSize)
				.limit((totalCount + pageSize - 1) / pageSize) // Calculate the number of batches
				.map(offset -> CompletableFuture.supplyAsync(() -> fetchProposalData(query, pageSize, offset),
						executorService))
				.map(futureProposals -> futureProposals.thenAccept(this::saveProposalsToNeo4j))
				.collect(Collectors.toList());

		// Wait for all CompletableFuture instances to complete
		CompletableFuture.allOf(futureList.toArray(CompletableFuture[]::new)).join();

		// Shutdown the executor service
		executorService.shutdown();
		var end = Duration.between(start, Instant.now()).toMillis();
		logger.debug("--------- Import eps_proposal: Total duration ------------ " + end);
	}

	private List<Proposal> fetchProposalData(String query, int pageSize, int offset) {
		String paginatedQuery = query + " LIMIT " + pageSize + " OFFSET " + offset;
		return jdbcTemplate.query(paginatedQuery, (resultSet, rowNum) -> {
			Proposal proposal = Proposal.builder().id(resultSet.getString("ID"))
					.proposalId(resultSet.getString("PROPOSAL_ID"))
					.leadUnitNumber(resultSet.getString("LEAD_UNIT_NUMBER"))
					.proposalStatus(resultSet.getString("PROPOSAL_STATUS"))
					.title(resultSet.getString("TITLE"))
					.startDate(resultSet.getString("START_DATE")).endDate(resultSet.getString("END_DATE"))
					.sponsorCode(resultSet.getString("SPONSOR_CODE")).sponsorName(resultSet.getString("SPONSOR_NAME"))
					.primeSponsorCode(resultSet.getString("PRIME_SPONSOR_CODE"))
					.primeSponsorName(resultSet.getString("PRIME_SPONSOR_NAME"))
					.grantHeaderId(resultSet.getString("GRANT_HEADER_ID"))
					.ipNumber(resultSet.getString("IP_NUMBER"))
					.unitNumber(resultSet.getString("UNIT_NUMBER")).leadUnitName(resultSet.getString("LEAD_UNIT_NAME"))
					.typeOfFunding(resultSet.getString("TYPE_OF_FUNDING_AGENCY")).piName(resultSet.getString("PI_NAME"))
					.build();
			return proposal;
		});
	}

	private void saveProposalsToNeo4j(List<Proposal> proposals) {
		proposalRepository.saveAll(proposals);
	}
/*----------------------------------------------------------------------------Import eps_proposal--------------------------------------------------------------------------------------------------------------*/

	private void deleteMedusaRelationships() {
		String cypherQuery = "MATCH ()-[r:INITIATES | RELATES_TO | RESULTS_IN | APPROVED_AS | PD_TO_SR | ASSOCIATED_WITH | FUNDED_BY | AGREEMENT_TO_SR | AGREEMENT_TO_AGREEMENT | SR_TO_SR | CHILD_AWARDS]->() DELETE r";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

	private void deleteMedusaNodes() {
		String cypherQuery = "MATCH (n:Agreement:Person:GrantCall:Proposal:InstituteProposal:ServiceRequest:Award:ACProtocol:IRBProtocol) DETACH DELETE n";
		neo4jClient.query(cypherQuery).in(schema).fetchAs(Map.class).all();
	}

}
