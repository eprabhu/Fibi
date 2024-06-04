import {APP_INITIALIZER, NgModule} from '@angular/core';
import {BrowserModule} from '@angular/platform-browser';

import {AppRoutingModule} from './app-routing.module';
import {AppComponent} from './app.component';
import {HashLocationStrategy, LocationStrategy} from "@angular/common";
import {CommonService} from "./common/services/common.service";
import {HTTP_INTERCEPTORS, HttpClientModule} from "@angular/common/http";
import {HeaderComponent} from "./common/header/header.component";
import {MatIconModule} from "@angular/material/icon";
import {AppRouterComponent} from "./common/app-router/app-router.component";
import {FooterComponent} from "./common/footer/footer.component";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import {AppHttpInterceptor} from './common/services/http-interceptor';
import {DashboardGuardService} from './common/services/dashboard-guard.service';
import {NavigationService} from './common/services/navigation.service';
import {EntityManagementGuardService} from './entity-management/entity-management-guard.service';
import {AdminRouteGuardService} from './common/services/guards/admin-route-guard.service';
import { LeftNavBarComponent } from './common/left-nav-bar/left-nav-bar.component';
import {MatMenuModule} from "@angular/material/menu";
import { SharedComponentModule } from './shared-components/shared-component.module';
import { FormsModule } from '@angular/forms';
import { DragDirective } from './common/header/drag.directive';
import { AddAttachmentModalModule } from './common/header/add-attachment-modal/add-attachment-modal.module';
import {HeaderService} from "./common/header/header.service";
import { LoginGuard } from './common/services/guards/login-guard.service';
import { ElasticConfigService } from './common/services/elastic-config.service';
import { InformationAndHelpTextService } from './common/services/informationAndHelpText.service';
import { SharedModule } from './shared/shared.module';

export function getappConfiguration(appConfigurationServiceService: CommonService) {
    return () => appConfigurationServiceService.getAppConfig();
}

@NgModule({
    declarations: [
        AppComponent,
        HeaderComponent,
        AppRouterComponent,
        FooterComponent,
        LeftNavBarComponent,
        DragDirective
    ],
    imports: [
        BrowserModule,
        BrowserAnimationsModule,
        AppRoutingModule,
        HttpClientModule,
        SharedComponentModule,
        MatIconModule,
        MatMenuModule,
        SharedModule,
        FormsModule,
        AddAttachmentModalModule
    ],
    providers: [CommonService,
        HeaderService,
        DashboardGuardService,
        ElasticConfigService,
        EntityManagementGuardService,
        {
            provide: APP_INITIALIZER,
            useFactory: getappConfiguration,
            deps: [CommonService],
            multi: true
        },
        {
            provide: HTTP_INTERCEPTORS,
            useClass: AppHttpInterceptor,
            multi: true
        }, {
            provide: LocationStrategy,
            useClass: HashLocationStrategy
        }, NavigationService,
        AdminRouteGuardService,
        LoginGuard,
        InformationAndHelpTextService],
    bootstrap: [AppComponent]
})
export class AppModule {


}
