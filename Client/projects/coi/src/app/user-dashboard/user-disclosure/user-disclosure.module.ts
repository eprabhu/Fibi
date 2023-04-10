import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {UserDisclosureComponent} from './user-disclosure.component';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";
import {UserDisclosureService} from "./user-disclosure.service";
import {SharedModule} from "../../shared/shared.module";
import {SharedComponentModule} from "../../shared-components/shared-component.module";
import {FormsModule} from "@angular/forms";

const routes: Routes = [{path: '', component: UserDisclosureComponent}];

@NgModule({
    declarations: [
        UserDisclosureComponent,
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        SharedModule,
        SharedComponentModule,
        FormsModule
    ],
    providers: [UserDisclosureService]
})
export class UserDisclosureModule {
}
