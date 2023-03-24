import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RelationshipComponent} from './relationship.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{path: '', component: RelationshipComponent}];

@NgModule({
    declarations: [
        RelationshipComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class RelationshipModule {
}
