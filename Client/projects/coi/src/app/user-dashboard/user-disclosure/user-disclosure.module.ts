import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";
import {UserDisclosureService} from "./user-disclosure.service";
import {SharedComponentModule} from "../../shared-components/shared-component.module";
import {FormsModule} from "@angular/forms";
import { SharedModule } from '../../shared/shared.module';
import { UserDisclosureComponent } from './user-disclosure.component';
import { DisclosureHistoryCardComponent } from './disclosure-history-card/disclosure-history-card.component';

const routes: Routes = [{path: '', component: UserDisclosureComponent}];

@NgModule({
    declarations: [
        UserDisclosureComponent,
        DisclosureHistoryCardComponent
    ],
    providers: [UserDisclosureService],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        SharedModule,
        SharedComponentModule,
        FormsModule,
    ]
})
export class UserDisclosureModule {
}
