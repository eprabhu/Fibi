package com.polus.apigateway.filter;

import java.util.List;
import java.util.function.Predicate;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.stereotype.Component;

@Component
public class RouteValidator {

//	@Value("${localhost.address}")
//	private String localhost;

	public static final List<String> openApiEndpoints = List.of("/auth/login", "/auth/logout", "/eureka", "/excelity",
			"/manpowerBaseSalary", "/sapConcur", "/processClaimInvoiceFeedResponseOOE",
			"/fastIntegrationRevenuePayments", "/processClaimInvoiceFeedRequestOOE");

	public Predicate<ServerHttpRequest> isSecured = request -> openApiEndpoints.stream()
			.noneMatch(uri -> request.getURI().getPath().contains(uri));

//	public Predicate<ServerHttpRequest> isSecured = request -> {
//		String path = request.getURI().getPath();
//		if (openApiEndpoints.contains(path)) {
//			if (path.startsWith("/sapConcur") || path.startsWith("/processClaimInvoiceFeedResponseOOE")) {
//				return request.getRemoteAddress() != null
//						&& request.getRemoteAddress().getHostString().equals("localhost");
//			}
//			return true;
//		}
//		return false;
//	};
}

