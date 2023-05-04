import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {UserDisclosureComponent} from './user-disclosure.component';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";
import {UserDisclosureService} from "./user-disclosure.service";
import {SharedComponentModule} from "../../shared-components/shared-component.module";
import {FormsModule} from "@angular/forms";
import { CoiSharedModule } from "../../disclosure/shared/shared.module";
import { SharedModule } from '../../shared/shared.module';

const routes: Routes = [{path: '', component: UserDisclosureComponent}];

@NgModule({
    declarations: [
        UserDisclosureComponent,
    ],
    providers: [UserDisclosureService],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        SharedModule,
        SharedComponentModule,
        FormsModule,
        CoiSharedModule
    ]
})
export class UserDisclosureModule {
}
