import {APP_INITIALIZER, NgModule} from '@angular/core';
import {BrowserModule} from '@angular/platform-browser';

import {AppRoutingModule} from './app-routing.module';
import {AppComponent} from './app.component';
import {HashLocationStrategy, LocationStrategy} from "@angular/common";
import {CommonService} from "./common/services/common.service";
import {HttpClientModule} from "@angular/common/http";
import {SharedModule} from "../../../fibi/src/app/shared/shared.module";
import {HeaderComponent} from "./common/header/header.component";
import {MatIconModule} from "@angular/material/icon";
import {AppRouterComponent} from "./common/app-router/app-router.component";
import {FooterComponent} from "./common/footer/footer.component";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";

export function getappConfiguration(appConfigurationServiceService: CommonService) {
    return () => appConfigurationServiceService.getAppConfig();
}

@NgModule({
    declarations: [
        AppComponent,
        HeaderComponent,
        AppRouterComponent,
        FooterComponent
    ],
    imports: [
        BrowserModule,
        BrowserAnimationsModule,
        AppRoutingModule,
        HttpClientModule,
        SharedModule,
        MatIconModule
    ],
    providers: [CommonService,
        {
            provide: APP_INITIALIZER,
            useFactory: getappConfiguration,
            deps: [CommonService],
            multi: true
        }, {provide: LocationStrategy, useClass: HashLocationStrategy}],
    bootstrap: [AppComponent]
})
export class AppModule {


}
