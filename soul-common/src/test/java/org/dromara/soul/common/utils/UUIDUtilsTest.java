package org.dromara.soul.common.utils;

import static org.mockito.Matchers.anyInt;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

import org.dromara.soul.common.utils.UUIDUtils;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.Random;

@RunWith(PowerMockRunner.class)
public class UUIDUtilsTest {

  @Rule
  public final ExpectedException thrown = ExpectedException.none();

  @PrepareForTest({Random.class, UUIDUtils.class, System.class})
  @Test
  public void testGenerateShortUuid() throws Exception {
    PowerMockito.mockStatic(System.class);

    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(13).thenReturn(30);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);
    PowerMockito.when(System.currentTimeMillis()).thenReturn(1_515_585_600_000L).thenReturn(-1L);

    Assert.assertEquals("951061054882631680", UUIDUtils.generateShortUuid());
    Assert.assertEquals("-5405765689543892943", UUIDUtils.generateShortUuid());
  }

  @PrepareForTest({Random.class, UUIDUtils.class, System.class})
  @Test
  public void testGenerateShortUuidClockMovedBackwardsException() throws Exception {
    PowerMockito.mockStatic(System.class);

    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(13);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);
    PowerMockito.when(System.currentTimeMillis()).thenReturn(-2L);

    thrown.expect(RuntimeException.class);
    UUIDUtils.generateShortUuid();
  }

  @PrepareForTest({Random.class, UUIDUtils.class})
  @Test
  public void testWorkerIdTooLarge() throws Exception {
    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(32);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);

    thrown.expect(IllegalArgumentException.class);
    UUIDUtils.generateShortUuid();
  }

  @PrepareForTest({Random.class, UUIDUtils.class})
  @Test
  public void testWorkerIdTooSmall() throws Exception {
    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(-1);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);

    thrown.expect(IllegalArgumentException.class);
    UUIDUtils.generateShortUuid();
  }

  @PrepareForTest({Random.class, UUIDUtils.class})
  @Test
  public void testDatacenterIdTooLarge() throws Exception {
    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(13).thenReturn(32);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);

    thrown.expect(IllegalArgumentException.class);
    UUIDUtils.generateShortUuid();
  }

  @PrepareForTest({Random.class, UUIDUtils.class})
  @Test
  public void testDatacenterIdTooSmall() throws Exception {
    final Random random = PowerMockito.mock(Random.class);
    PowerMockito.when(random.nextInt(anyInt())).thenReturn(13).thenReturn(-1);
    PowerMockito.whenNew(Random.class).withNoArguments().thenReturn(random);

    thrown.expect(IllegalArgumentException.class);
    UUIDUtils.generateShortUuid();
  }
}
