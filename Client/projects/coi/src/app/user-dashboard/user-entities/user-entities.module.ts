import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {UserEntitiesComponent} from './user-entities.component';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";

const routes: Routes = [{path: '', component: UserEntitiesComponent}];

@NgModule({
    declarations: [
        UserEntitiesComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
    ]
})
export class UserEntitiesModule {
}
