import { RouterModule, Routes } from "@angular/router";
import { FaqComponent } from "./faq/faq.component";
import { NgModule } from "@angular/core";

const routes: Routes = [
    { path: 'faq', component: FaqComponent }
]
@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule],
    providers: []
})
export class UserRoutingModule { }
