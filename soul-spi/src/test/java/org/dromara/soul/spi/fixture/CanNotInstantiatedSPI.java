package org.dromara.soul.spi.fixture;

import org.dromara.soul.spi.Join;

/**
 * can't instantiated test SPI class.
 *
 * @author wyc192273
 */
@Join
public final class CanNotInstantiatedSPI implements JdbcSPI {

    private CanNotInstantiatedSPI() {
    }

    @Override
    public String getClassName() {
        return "canNotInstantiatedSPI";
    }
}
