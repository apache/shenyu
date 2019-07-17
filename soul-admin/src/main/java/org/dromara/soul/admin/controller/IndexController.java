/*
 *
 * Copyright 2017-2018 549477611@qq.com(xiaoyu)
 *
 * This copyrighted material is made available to anyone wishing to use, modify,
 * copy, or redistribute it subject to the terms and conditions of the GNU
 * Lesser General Public License, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this distribution; if not, see <http://www.gnu.org/licenses/>.
 *
 */

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.listener.SoulDomain;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;


/**
 * The type Index controller.
 *
 * @author xiaoyu
 */
@Controller
public class IndexController {

    /**
     * Index string.
     *
     * @param model the model
     * @return the string
     */
    @RequestMapping("/index")
    public String index(final Model model) {
        model.addAttribute("domain", SoulDomain.getInstance().getHttpPath());
        return "index";
    }

}
